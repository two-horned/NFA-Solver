{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Exception (catch, throwIO)
import Control.Monad (liftM2, ap)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import qualified Data.CharSet as C
import qualified Data.HashMap.Strict as H
import qualified Data.IntSet as I
import NFA (NFA, calcNFA)
import System.IO.Error (isEOFError)
import System.Posix.IO (stdInput)
import System.Posix.Terminal (queryTerminal)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

regexToNFA :: String -> Maybe (NFA Char)
regexToNFA rg =
  let -- Limit the alphabet to ASCII Letters and Digits.
      alph = digs ++ ['A' .. 'Z'] ++ ['a' .. 'z'] ++ specU ++ specN
      alphU = ['A' .. 'Z']
      alphL = ['a' .. 'z']
      digs = ['0' .. '9']
      specU = ['+', '?', '*', '+', '|', '.', '\\', '(', ')', '[', ']', '!']
      specN = [' ', ',', ':', ';', '@', '$', '#', '<', '>', '&', '^', '%' ]

      specUS = C.fromList specU
      specNS = C.fromList specN
      alphS = C.fromList alph

      -- The epsilon symbol is reserved to mark empty transitions.
      eps = 'ε'

      isVal c = isAsciiLower c || isAsciiUpper c || isDigit c || C.member c specNS

      isPair a
        | isAsciiLower a = isAsciiLower
        | isAsciiUpper a = isAsciiUpper
        | isDigit a = isDigit
        | otherwise = const False

      gdert x z y = H.insertWith I.union (x, y) (I.singleton z)
      multicon = flip . foldr . ap gdert (+1)
      edert x y = gdert x y eps
      report = curry $ (,,) . Just

      expand acc = \case
        ']' : xs -> Just (acc, xs)
        a : '-' : b : xs
          | isPair a b -> expand ([a .. b] <> acc) xs
        '\\' : c : xs | C.member c specUS -> expand (c : acc) xs
        c : xs | isVal c -> expand (c : acc) xs
        _ -> Nothing

      parseE p = \case
        '^' : ']' : xs -> report '^' xs
        ']' : xs -> report ']' xs
        '^' : xs -> case expand [] xs of
          Just (e, u) ->
            let e' = C.toList $ alphS C.\\ C.fromList e
            in \n -> parseN p n (n + 1) u . multicon n e'
          _ -> report '^' xs
        xs -> case expand [] xs of
          Just (e, u) -> \n -> parseN p n (n + 1) u . multicon n e
          _ -> report '[' xs

      parseN p o n = \case
        '?' : xs -> parseN p o n xs . edert o n
        '+' : xs -> parseN p o n xs . edert n o
        '*' : xs -> parseN p o n xs . edert n o . edert o n
        '|' : xs -> (\(t, n', mp'') -> (t, n', edert n n' mp''))
            . parseS p (n + 1) xs
            . edert p (n + 1)
        xs -> parseS p n xs

      parseS o n = \case
        '[' : xs -> parseE o xs n
        '.' : xs -> parseN o n (n + 1) xs . multicon n alph
        '\\' : 'd' : xs -> parseN o n (n + 1) xs . multicon n digs
        '\\' : 'a' : xs -> parseN o n (n + 1) xs . multicon n alphL
        '\\' : 'A' : xs -> parseN o n (n + 1) xs . multicon n alphU
        '\\' : c : xs | C.member c specUS -> parseN o n (n + 1) xs . multicon n [c]
        ('(' : xs) -> (\case
                (Just (')', ys), n', mp') -> parseN o n n' ys mp'
                (_, n', mp') -> report '(' xs n' mp') . parseS n n xs
        c : xs | isVal c -> parseN o n (n + 1) xs . multicon n [c]
        c : xs -> report c xs n
        _ -> (,,) Nothing n
   in case parseS 0 0 rg H.empty of
        (Nothing, n, mp) -> Just (eps, 0, mp, I.singleton n)
        _ -> Nothing

type Logger = Bool -> String -> IO ()

printWelcome :: Logger -> IO ()
printWelcome lgr =
  lgr
    False
    "Welcome! This is a simple demonstration that demonstrates\n\
    \ my algorithm, which checks input sequences for an NFA, works\n\
    \ well for validating strings with Regular Expressions.\n\n\
    \ First, we read a form of Regular Expression, convert them\n\
    \ to an NFA and then check for various inputs. Please note\n\
    \ the syntax is limited to the ASCII alphabet & digits, parenthesis,\n\
    \ and symbols '.', '?', '+', '*', '|', '[', ']', '^', '\\d'\n\
    \ which represent the commonly known operations in Regular Expressions.\n\n\
    \ Please note, according to our precedence rules concatenation comes before alternation.\n\
    \ My algorithm is not limited to Regular Expressions. One only needs to find a way to translate\n\
    \ a problem to NFA input validation and define a parser for the appropriate NFA."

getNFA :: Logger -> IO (NFA Char)
getNFA lgr = go
  where
    go = do
      lgr False "Provide Regex (input '!' to change later)."
      getLine >>= maybe handleB handleA . regexToNFA
    handleA nfa = lgr True "Valid Regex." >> return nfa
    handleB = lgr True "Invalid Regex." >> go

checkInputs :: Logger -> NFA Char -> IO ()
checkInputs lgr nfa = go
  where
    lgrA = lgr True "Accepted."
    lgrB = lgr True "Rejected."
    ca = calcNFA nfa
    ag = agenda lgr
    go = do
      lgr False "Feed input words line by line.\n\n"
      getLine >>= (\case "!" -> ag; w | ca w -> lgrA; _ -> lgrB) >> go

sayBye :: IO ()
sayBye = putStrLn "Bye."

agenda :: Logger -> IO ()
agenda = liftM2 (>>=) getNFA checkInputs

main :: IO ()
main = do
  isTTY <- queryTerminal stdInput
  let prF = if isTTY then putStrLn else const $ return ()
  let lgr = \case True -> putStrLn; False -> prF
  printWelcome lgr
  start <- getCurrentTime
  agenda lgr `catch` (\e -> if isEOFError e then sayBye else throwIO e)
  getCurrentTime >>= putStrLn . show . flip diffUTCTime start
