{-# LANGUAGE LambdaCase #-}

module Main where

import System.Posix.Terminal (queryTerminal)
import System.Posix.IO (stdOutput)
import System.IO.Error (isEOFError)
import Control.Exception (catch, throwIO)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import qualified Data.CharSet as C
import qualified Data.HashMap.Strict as H
import qualified Data.IntSet as I
import NFA (calcNFA, NFA)

regexToNFA :: String -> Maybe (NFA Char)
regexToNFA rg =
  let gdert = H.insertWith I.union
      -- Limit the alphabet to ASCII Letters and Digits.
      alph = digs ++ ['A' .. 'Z'] ++ ['a' .. 'z']
      digs = [ '0' .. '9' ]
      alphS = C.fromList alph

      -- The epsilon symbol is reserved to mark empty transitions.
      eps = 'ε'

      isVal c = isAsciiLower c || isAsciiUpper c || isDigit c

      isPair a
        | isAsciiLower a = isAsciiLower
        | isAsciiUpper a = isAsciiUpper
        | isDigit a = isDigit
        | otherwise = const False

      multicon xs n mp =
        let ii bp x = gdert (n, x) (I.singleton (n + 1)) bp
         in foldl' ii mp xs

      expand ip = case ip of
        ']' : xs -> Just ([], xs)
        a : '-' : b : xs | isPair a b -> do
          (r, u) <- expand xs
          return ([a .. b] ++ r, u)
        c : xs | isAsciiLower c || isAsciiUpper c || isDigit c -> do
          (r, u) <- expand xs
          return (c : r, u)
        _ -> Nothing

      parseE ip n mp = case ip of
        '^' : ']' : xs -> (mp, n, Just ('^', xs))
        '^' : xs ->
          case expand xs of
            Just (e, u) ->
              let e' = C.toList $ alphS C.\\ C.fromList e
               in parseN u n (n + 1) (multicon e' n mp)
            _ -> (mp, n, Just ('^', xs))
        ']' : xs -> (mp, n, Just (']', xs))
        xs -> case expand ip of
          Just (e, u) -> parseN u n (n + 1) (multicon e n mp)
          _ -> (mp, n, Just ('[', xs))

      parseN ip o n mp = case ip of
        '?' : xs -> parseN xs o n $ gdert (o, eps) (I.singleton n) mp
        '+' : xs -> parseN xs o n $ gdert (n, eps) (I.singleton o) mp
        '*' : xs -> parseN ('+' : '?' : xs) o n mp
        '|' : xs ->
          let mp' = gdert (o, eps) (I.singleton (n + 1)) mp
              (mp'', n', t) = parseS xs (n + 1) mp'
           in (gdert (n, eps) (I.singleton n') mp'', n', t)
        _ -> parseS ip n mp

      parseS ip n mp = case ip of
        ('(' : xs) ->
          let (mp', n', t) = parseS xs n mp
           in case t of
                Just (')', ys) -> parseN ys n n' mp'
                _ -> (mp', n', Just ('(', xs))
        '[' : xs -> parseE xs n mp
        '.' : xs -> parseN xs n (n + 1) (multicon alph n mp)
        '\\' : 'd' : xs -> parseN xs n (n + 1) (multicon digs n mp)
        c : xs
          | isVal c ->
              parseN xs n (n + 1) (multicon [c] n mp)
        c : xs -> (mp, n, Just (c, xs))
        _ -> (mp, n, Nothing)
   in case parseS rg 0 H.empty of
        (mp, n, Nothing) -> Just (eps, 0, mp, I.singleton n)
        _ -> Nothing

type Logger = String -> IO ()

printWelcome :: Logger -> IO ()
printWelcome log =
  log "Welcome! This is a simple demonstration that demonstrates\n\
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
getNFA log = go
    where
      go = do
        log "Provide Regex (input '!' to change later)."
        getLine >>= maybe handleB handleA . regexToNFA
      handleA nfa = log "Valid Regex." >> return nfa
      handleB     = log "Invalid Regex." >> go


checkInputs :: Logger -> NFA Char -> IO ()
checkInputs log nfa = go
  where
    logA = log "Accepted."; logB = log "Rejected."
    ca = calcNFA nfa; ag = agendaSlim log
    go = do
      log "Feed input words line by line.\n\n"
      getLine >>= \case; "!" -> ag; w | ca w -> logA; _ -> logB >> go


sayBye :: IO ()
sayBye = putStrLn "Bye."


agendaSlim :: Logger -> IO ()
agendaSlim log = getNFA log >>= checkInputs log


agenda :: IO ()
agenda = do
  isTTY <- queryTerminal stdOutput
  let log = if isTTY then putStr else const $ return ()
  printWelcome log >> agendaSlim log

main :: IO ()
main = agenda `catch` (\e -> if isEOFError e then sayBye else throwIO e)
