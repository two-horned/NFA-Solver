module Main where

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
      alph = ['0' .. '9'] ++ ['A' .. 'Z'] ++ ['a' .. 'z']
      alphS = C.fromList alph
      eps = 'ε'

      isVal c = isAsciiLower c || isAsciiUpper c || isDigit c

      isPair a b
        | isAsciiLower a = isAsciiLower b
        | isAsciiUpper a = isAsciiUpper b
        | isDigit a = isDigit b
        | otherwise = False

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
        c : xs
          | isVal c ->
              parseN xs n (n + 1) (multicon [c] n mp)
        c : xs -> (mp, n, Just (c, xs))
        _ -> (mp, n, Nothing)
   in case parseS rg 0 H.empty of
        (mp, n, Nothing) -> Just (eps, 0, mp, I.singleton n)
        _ -> Nothing


getNFA :: IO (NFA Char)
getNFA = do
  putStr "Provide Regex (input '!' to change later).\n\n"
  regex <- getLine
  case regexToNFA regex of
    Nothing -> do
      putStr "Invalid Regex.\n\n"
      getNFA
    Just nfa -> do
      putStr "Valid Regex.\n\n"
      return nfa

checkInputs :: NFA Char -> IO ()
checkInputs nfa = do
  putStr "Feed input words line by line.\n\n"
  word <- getLine
  if word == "!"
    then agenda
    else do
      if calcNFA nfa word
        then putStr "Accepted.\n\n"
        else putStr "Rejected.\n\n"
      checkInputs nfa

sayBye :: IO ()
sayBye = putStrLn "Bye."

agenda :: IO ()
agenda = do
  nfa <- getNFA
  checkInputs nfa

main :: IO ()
main = agenda `catch` (\e -> if isEOFError e then sayBye else throwIO e)
