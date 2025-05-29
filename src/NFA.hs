module NFA where

import qualified Data.HashMap.Strict as M
import Data.Hashable
import qualified Data.IntSet as S

type NFA a =
  ( a, -- Epsilon
    Int, -- Initial State
    M.HashMap (Int, a) S.IntSet, -- Transitions
    S.IntSet -- Final States
  )

-- | Calculate a word using Said's NFA-Algorithm.
--   Runtime is linear regarding the word length,
--   and polynomial regarding the amount of states.
calcNFA :: (Hashable a) => NFA a -> [a] -> Bool
calcNFA nfa = cleanup . go
  where
    (eps, i, trs, fin) = nfa
    look w s = M.findWithDefault S.empty (s, w) trs
    sfoldMap f = S.foldl' (\b a -> f a <> b) mempty -- remove when container is v0.8
    kids w = sfoldMap (look w)
    growgen gs = let ns = flip (S.\\) <*> kids eps $ gs
        in if S.null ns then gs else growgen (gs <> ns)
    nextgen w = kids w . growgen
    cleanup = not . S.disjoint fin . growgen
    go = foldl' (flip nextgen) (S.singleton i)
