module Main where

import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as H
import qualified Data.IntSet as I
import Data.List
import NFA

-- | Split list at first occurance of an item and
--   return Nothing if no such item exists.
splitFirst :: (Eq a) => a -> [a] -> Maybe ([a], [a])
splitFirst = go id
  where
    go _ _ [] = Nothing
    go acc c (x:xs)
      | c == x = Just (acc [], xs)
      | otherwise = go (acc . (x :)) c xs

-- | Split list at first occurance of a defined right bracket
--   and nest other defined left brackets and right brackets.
--   Return Nothing if a right bracket is missing.
splitRight :: (Eq a) => a -> a -> [a] -> Maybe ([a], [a])
splitRight = go (0 :: Int) id
  where
    go _ _ _ _ [] = Nothing
    go i acc l r (x:xs)
      | l == x = go (i + 1) acc l r xs
      | r /= x = go i (acc . (x :)) l r xs
      | i > 0 = go (i - 1) (acc . (x :)) l r xs
      | otherwise = Just (acc [], xs)

regexToNFA :: String -> Maybe (NFA Char)
regexToNFA rg =
  let alph = alph1 ++ alph2 ++ alph3
      alph1 = "abcdefghijklmnopqrstuvwxyz"
      alph2 = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
      alph3 = "0123456789"
      gdert = M.insertWith I.union
      eps = 'ε'
      expand [] = Just []
      expand (x:'-':y:zs) = do
        as <- find (elem x) [alph1, alph2, alph3]
        (ls, _) <- splitFirst y $ dropWhile (/= x) as
        rs <- expand zs
        return $ y : ls ++ rs
      expand (x:xs)
        | elem x alph = do
          ys <- expand xs
          return $ x : ys
        | otherwise = Nothing
      multicon xs n mp =
        let ii bp x = gdert (n, x) (I.singleton (n + 1)) bp
            go = foldl' ii mp xs
         in (go, n + 1)
      pass o n ('*':xs) mp =
        let mp' =
              gdert (o, eps) (I.singleton n) $ gdert (n, eps) (I.singleton o) mp
         in pass' o n xs mp'
      pass o n ('+':xs) mp =
        let mp' = gdert (n, eps) (I.singleton o) mp
         in pass' o n xs mp'
      pass o n ('?':xs) mp =
        let mp' = gdert (o, eps) (I.singleton n) mp
         in pass' o n xs mp'
      pass o n xs mp = pass' o n xs mp
      pass' o n ('|':x:xs) mp = do
        let mp' = gdert (o, eps) (I.singleton (n + 1)) mp
        (mp'', n') <- parse (x : xs) (n + 1) mp'
        return (gdert (n, eps) (I.singleton n') mp'', n')
      pass' _ n xs mp = parse xs n mp
      parse [] n mp = Just (mp, n)
      parse ('[':'^':xs) n mp = do
        (ls1, rs) <- splitFirst ']' xs
        ls2 <- expand ls1
        let ls3 = H.toList $ H.difference (H.fromList alph) (H.fromList ls2)
        let (mp', n') = multicon ls3 n mp
        pass n n' rs mp'
      parse ('[':xs) n mp = do
        (ls, rs) <- splitFirst ']' xs
        as <- expand ls
        let (mp', n') = multicon as n mp
        pass n n' rs mp'
      parse ('.':xs) n mp = do
        let (mp', n') = multicon alph n mp
        pass n n' xs mp'
      parse ('(':xs) n mp = do
        (ls, rs) <- splitRight '(' ')' xs
        (mp', n') <- parse ls n mp
        pass n n' rs mp'
      parse (x:xs) n mp
        | not $ H.member x (H.fromList alph) = Nothing
        | otherwise = do
          let (mp', n') = multicon [x] n mp
          pass n n' xs mp'
   in do
        (mp, n) <- parse rg 0 M.empty
        return (eps, 0, mp, I.singleton n)

main :: IO ()
main = do
  putStr "Input Regex (Input '!' word to change later):\n"
  regex <- getLine
  case regexToNFA regex of
    Nothing -> do
      putStr "Not a valid regex\n"
      main
    Just nfa -> niam nfa
  where
    niam nfa = do
      putStr "Input word:\n"
      word <- getLine
      if word == "!"
        then main
        else if calcNFA nfa word
               then putStr "Word accepted\n"
               else putStr "Word rejected\n"
      niam nfa
