--------------------------------------------------------------------------------
-- Functional Programming
--------------------------------------------------------------------------------
-- Copyright (c) 2022 Michael B. Gale (michael@fpclass.online)
--
-- This source code is subject to the terms and conditions found in the LICENSE
-- file in the root directory of this source tree.
--------------------------------------------------------------------------------

{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lab where

--------------------------------------------------------------------------------

import Prelude hiding (Word)

import Control.Monad.State

import Data.Map qualified as M
import Data.List (intersect, union)
import Data.Set qualified as S

--------------------------------------------------------------------------------

-- | A `Word` is a list of symbols @s@.
type Word s = [s]

-- | A type to represent DFAs where @q@ is the type of states and @s@ is the
-- type of symbols.
data DFA q s
    = MkDFA {
        -- | The initial state of the DFA.
        dfaInitialState :: q,
        -- | `dfaDelta` @state symbol@ is the DFA's transition function which
        -- determines the state resulting from a transition for @symbol@
        -- starting at @state@.
        dfaDelta :: q -> s -> q,
        -- | The list of accepting states of the DFA.
        dfaAcceptingStates :: [q]
    }

--------------------------------------------------------------------------------

-- | Represents the binary alphabet.
data Binary = Zero | One
    deriving (Eq, Show, Enum, Bounded)

-- | Represents states for the DFA which accepts zero or more `Zero`s, followed
-- by one or more `One`s.
data DFAState = Q0 | Q1 | Q2
    deriving (Eq, Show, Enum, Bounded)

-- | `dfa0` is a DFA which accepts zero or more `Zero`s, followed by one or
-- more `One`s.
dfa0 :: DFA DFAState Binary
dfa0 = MkDFA{
    dfaInitialState = undefined,
    dfaDelta = delta,
    dfaAcceptingStates = undefined
} where
    delta :: DFAState -> Binary -> DFAState
    delta = undefined

-- | `dfaAccepts` @dfa word@ determines whether the @dfa@ accepts @word@.
dfaAccepts :: Eq q => DFA q s -> Word s -> Bool
dfaAccepts (MkDFA initial delta accept) w = undefined
    where deltaHat _ _ = undefined

--------------------------------------------------------------------------------

-- | A Type to represent NFAs where @q@ is the type of states and @s@ is the
-- type of symbols.
data NFA q s
    = MkNFA {
        -- | The initial states of the NFA.
        nfaInitialStates :: [q],
        -- | `nfaDelta` @states symbol@ is the NFA's transition function which
        -- determines the states resulting from transitions for @symbol@
        -- starting at @states@.
        nfaDelta :: q -> s -> [q],
        -- | The list of accepting states of the NFA.
        nfaAcceptingStates :: [q]
    }

--------------------------------------------------------------------------------

-- | Represents the first four characters of the alphabet.
data SmallAlphabet = A | B | C | D
    deriving (Eq, Ord, Show, Enum, Bounded)

-- | Represents states for the NFA below.
data NFAState = S0 | S1 | S2 | S3
    deriving (Eq, Show, Enum, Bounded)

-- | `nfa0` is a NFA which accepts words that:
-- - Start with one or more A -or- one B
-- - Contain a mix of zero or more B, C, or D
-- - End in a D
nfa0 :: NFA NFAState SmallAlphabet
nfa0 = undefined
    where delta :: NFAState -> SmallAlphabet -> [NFAState]
          delta _ _ = undefined

-- | `nfaAccepts` @nfa word@ determines whether the @nfa@ accepts @word@.
nfaAccepts :: Eq q => NFA q s -> Word s -> Bool
nfaAccepts (MkNFA initials delta accept) w = undefined

--------------------------------------------------------------------------------

class Automaton a where
    accepts :: Eq q => a q s -> Word s -> Bool

instance Automaton DFA where
    accepts = dfaAccepts

instance Automaton NFA where
    accepts = nfaAccepts

--------------------------------------------------------------------------------

-- | `unions` @lists@ calculates the union of all the sub-lists of @lists@.
unions :: Eq a => [[a]] -> [a]
unions = undefined

-- | `powerset` @list@ calculates the powerset of @list@.
powerset :: [a] -> [[a]]
powerset = undefined

-- | `nfa2dfa` @nfa@ converts @nfa@ into a `DFA`.
nfa2dfa
    :: forall q s . (Eq q, Enum q, Bounded q)
    => NFA q s -> DFA [q] s
nfa2dfa (MkNFA initials delta accept) = MkDFA{
    dfaInitialState = undefined,
    dfaDelta = deltaDFA,
    dfaAcceptingStates = undefined
} where -- | `states` are the states of the `DFA` that we obtain through
        -- subset construction.
        states :: [[q]]
        states = powerset [minBound..maxBound]

        -- | `deltaDFA` @dfaState symbol@ is the transition function of the
        -- resulting `DFA`.
        deltaDFA :: [q] -> s -> [q]
        deltaDFA qs x = undefined

--------------------------------------------------------------------------------

-- | A mapping from pairs of states and symbols to the resulting set of states,
-- used to represent transitions.
type Transitions q s = M.Map (q,s) (S.Set q)

-- | For the next exercise we need a more efficient representation of NFAs
-- where it is also easier for us to inspect transitions.
data NFA2 q s
    = MkNFA2 {
        -- | The set of initial states for the NFA.
        nfa2InitialStates :: S.Set q,
        -- | The set of final states for the NFA.
        nfa2AcceptingStates :: S.Set q,
        -- | The transitions of the NFA.
        nfa2Transitions :: Transitions q s
    } deriving (Eq, Show)

-- | `nfa2nfa` @nfa@ converts a `NFA2` value to an equivalent `NFA` value.
nfa2nfa :: forall q s . (Ord q, Ord s) => NFA2 q s -> NFA q s
nfa2nfa (MkNFA2 is fs trans) = MkNFA{
    nfaInitialStates = S.toList is,
    nfaAcceptingStates = S.toList fs,
    nfaDelta = delta
} where delta :: q -> s -> [q]
        delta q s = case M.lookup (q,s) trans of
            Nothing -> []
            Just qs -> S.toList qs

-- | Represents regular expressions.
data RegEx s
    = Empty
    | Epsilon
    | Symbol s
    | Plus (RegEx s) (RegEx s)
    | Concat (RegEx s) (RegEx s)
    | Star (RegEx s)
    deriving (Eq, Show)

-- | `regExp0` is a `RegEx` representing the same language as `nfa0`.
regExp0 :: RegEx SmallAlphabet
regExp0 = ((Symbol A `Concat` Star (Symbol A)) `Plus` Symbol B)
    `Concat` Star (Symbol B `Plus` Symbol C `Plus` Symbol D)
    `Concat` Symbol D

-- | `fresh` is a computation which generates a fresh identifier.
fresh :: State Int Int
fresh = StateT (\n -> pure (n, n+1))

-- | `mergeTransitions` @left right@ merges all transitions from @left@
-- and @right@. This is not just `M.union` since both @left@ and @right@
-- may contain the same keys, in which case we wish to combine their
-- sets of target states.
mergeTransitions
    :: (Ord q, Ord s)
    => Transitions q s -> Transitions q s -> Transitions q s
mergeTransitions = M.unionWith S.union

-- | `toFinal` @nfa@ finds a `S.Set` of all pairs of states and symbols which
-- have a transition to a final state in @nfa@.
toFinal :: (Ord q, Ord s) => NFA2 q s -> S.Set (q,s)
toFinal nfa =
    S.fromList [ p
               | (p, qs) <- M.toList (nfa2Transitions nfa)
               , qs `S.intersection` nfa2AcceptingStates nfa /= S.empty
               ]

-- | `toNFA` @regExp@ converts @regExp@ to an `NFA`.
toNFA :: Ord s => RegEx s -> NFA Int s
toNFA regex = nfa2nfa $ evalState (toNFA' regex) 0

-- | `toNFA'` @regExp@ is a computation which converts @regExp@ into an `NFA2`.
toNFA' :: forall s . Ord s => RegEx s -> State Int (NFA2 Int s)
toNFA' Empty = do
    node <- fresh

    pure MkNFA2{
        nfa2InitialStates = undefined,
        nfa2AcceptingStates = undefined,
        nfa2Transitions = undefined
    }
toNFA' Epsilon = do
    node <- fresh

    undefined
toNFA' (Symbol x) = do
    initial <- fresh
    final <- fresh

    undefined
toNFA' (Plus l r) = do
    lNFA <- toNFA' l
    rNFA <- toNFA' r

    undefined
toNFA' (Concat l r) = undefined
toNFA' (Star e) = undefined

--------------------------------------------------------------------------------
