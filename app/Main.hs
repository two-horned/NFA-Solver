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
      alph = digs ++ ['A' .. 'Z'] ++ ['a' .. 'z']
      digs = ['0' .. '9']
      alphS = C.fromList alph

      -- The epsilon symbol is reserved to mark empty transitions.
      eps = 'ε'

      isVal c = isAsciiLower c || isAsciiUpper c || isDigit c

      isPair a
        | isAsciiLower a = isAsciiLower
        | isAsciiUpper a = isAsciiUpper
        | isDigit a = isDigit
        | otherwise = const False

      gdert x z y = H.insertWith I.union (x, y) (I.singleton z)
      multicon = flip . foldr . ap gdert (+1)
      edert x y = gdert x y eps
      report s xs n mp = (Just (s, xs), n, mp)

      expand acc = \case
        ']' : xs -> Just (acc, xs)
        a : '-' : b : xs
          | isPair a b -> expand ([a .. b] <> acc) xs
        c : xs | isVal c -> expand (c : acc) xs
        _ -> Nothing

      parseE ip = case ip of
        '^' : ']' : xs -> report '^' xs
        ']' : xs -> report ']' xs
        '^' : xs -> case expand [] xs of
          Just (e, u) ->
            let e' = C.toList $ alphS C.\\ C.fromList e
            in \n -> parseN u n (n + 1) . multicon n e'
          _ -> report '^' xs
        xs -> case expand [] xs of
          Just (e, u) -> \n -> parseN u n (n + 1) . multicon n e
          _ -> report '[' xs

      parseN ip o n = case ip of
        '?' : xs -> parseN xs o n . edert o n
        '+' : xs -> parseN xs o n . edert n o
        '*' : xs -> parseN xs o n . edert n o . edert o n
        '|' : xs -> (\(t, n', mp'') -> (t, n', edert n n' mp''))
            . parseS xs (n + 1)
            . edert o (n + 1)
        _ -> parseS ip n

      parseS ip n = case ip of
        '[' : xs -> parseE xs n
        '.' : xs -> parseN xs n (n + 1) . multicon n alph
        '\\' : 'd' : xs -> parseN xs n (n + 1) . multicon n digs
        ('(' : xs) -> \mp ->
          let (t, n', mp') = parseS xs n mp
           in case t of
                Just (')', ys) -> parseN ys n n' mp'
                _ -> report '(' xs n' mp'
        c : xs
          | isVal c -> parseN xs n (n + 1) . multicon n [c]
        c : xs -> report c xs n
        _ -> \mp -> (,,) Nothing n mp
   in case parseS rg 0 H.empty of
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
    \ Please note, we do not have a fixed precedence rule for concatenation and alternation,\n\
    \ so please use paranthesis to group subexpressions together.\n\
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
