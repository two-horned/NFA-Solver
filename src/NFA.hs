module NFA where

import qualified Data.IntSet as S
import qualified Data.Map as M

type NFA a
  = ( a -- Epsilon
    , Int -- Initial State
    , M.Map (Int, a) S.IntSet -- Transitions
    , S.IntSet -- Final States
      )

-- | Calculate a word in the NFA the naive way (exponential runtime).
--   Not used, just written down for comparison.
naiveCalcNFA :: (Ord a) => NFA a -> [a] -> Bool
naiveCalcNFA nfa word = go word i S.empty
  where
    (eps, i, trs, fin) = nfa
    look k = M.findWithDefault S.empty k trs
    go [] s fs =
      let epi = look (s, eps) S.\\ fs
          bad = [go [] n (S.insert n fs) | n <- S.toList epi]
       in S.member s fin || or bad
    go ws@(w:ws') s fs =
      let gud = [go ws' n S.empty | n <- S.toList (look (s, w))]
          epi = look (s, eps) S.\\ fs
          bad = [go ws n (S.insert n fs) | n <- S.toList epi]
       in or $ gud ++ bad

-- | Calculate a word using Said's NFA-Algorithm.
-- Runtime is linear regarding to the word length,
-- and polynomial regarding to the amount of states.
calcNFA :: (Ord a) => NFA a -> [a] -> Bool
calcNFA nfa word = go word (S.singleton i)
  where
    (eps, i, trs, fin) = nfa
    look k = M.findWithDefault S.empty k trs
    concatMapS f = S.foldl (\b a -> S.union (f a) b) S.empty
    nextgen fs w s =
      let leg = look (s, w)
          gel = look (s, eps)
          fs' = S.insert s fs
          pro = concatMapS (nextgen fs' w) $ gel S.\\ fs
       in S.union leg pro
    go [] sts =
      not $ S.disjoint fin (S.union sts (concatMapS (nextgen S.empty eps) sts))
    go (w:ws) sts = go ws (concatMapS (nextgen S.empty w) sts)
