--------------------------------------------------------------------------------
-- Functional Programming
--------------------------------------------------------------------------------
-- Copyright (c) 2022 Michael B. Gale (michael@fpclass.online)
--
-- This source code is subject to the terms and conditions found in the LICENSE
-- file in the root directory of this source tree.
--------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

--------------------------------------------------------------------------------

import Data.List (nub)

import Test.Tasty
import Test.Tasty.Ingredients
import Test.Tasty.Ingredients.Basic
import Test.Tasty.Runners.AntXML
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Lab

--------------------------------------------------------------------------------

prop_dfaWords :: Property
prop_dfaWords = property $ do
    -- generate zero or more zeros
    zeros <- forAll $ Gen.list (Range.constant 0 100) $ pure Zero
    -- generate one or more ones
    ones <- forAll $ Gen.list (Range.constant 1 100) $ pure One
    -- check that the DFA accepts the zeros followed by the ones
    H.assert $ dfa0 `dfaAccepts` (zeros <> ones)

prop_rejectNoOnes :: Property
prop_rejectNoOnes = property $ do
    -- generate zero or more zeros
    zeros <- forAll $ Gen.list (Range.constant 0 100) $ pure Zero
    -- check that the DFA rejects the zeros
    H.assert $ not $ dfa0 `dfaAccepts` zeros

prop_rejectZerosAfterOnes :: Property
prop_rejectZerosAfterOnes = property $ do
    -- generate zero or more zeros
    zeros <- forAll $ Gen.list (Range.constant 0 100) $ pure Zero
    -- generate one or more ones
    ones <- forAll $ Gen.list (Range.constant 1 100) $ pure One
    -- generate more zeros
    moreZeros <- forAll $ Gen.list (Range.constant 1 100) $ pure Zero
    -- check that the DFA accepts the zeros followed by the ones
    H.assert $ not $ dfa0 `dfaAccepts` (zeros <> ones <> moreZeros)

dfa0Tests :: TestTree
dfa0Tests = testGroup "dfa0"
    [ testCase "rejects the empty word" $
        assertBool "The empty word is accepted" $ not $ dfa0 `dfaAccepts` []
    , testProperty
        "accepts zero or more zeros, followed by one or more ones"
        "prop_dfaWords"
        prop_dfaWords
    , testProperty
        "rejects zero or more zeros, followed by no ones"
        "prop_rejectNoOnes"
        prop_rejectNoOnes
    , testProperty
        "rejects zeros after ones"
        "prop_rejectZerosAfterOnes"
        prop_rejectZerosAfterOnes
    ]

--------------------------------------------------------------------------------

nfaStart :: MonadGen m => m [SmallAlphabet]
nfaStart = Gen.choice
    [ Gen.list (Range.constant 1 20) $ pure A
    , Gen.list (Range.singleton 1) $ pure B
    ]

nfaMiddle :: MonadGen m => m [SmallAlphabet]
nfaMiddle = Gen.list (Range.constant 0 100) $ Gen.element [B, C, D]

prop_nfaWords :: (Automaton a, Eq q) => a q SmallAlphabet -> Property
prop_nfaWords atmtn = property $ do
    -- generate one or more As or one B
    start <- forAll nfaStart
    -- generate zero or more Bs, Cs, or Ds
    middle <- forAll nfaMiddle
    -- construct and log the entire word
    let word = start <> middle <> [D]
    annotateShow word
    -- check that the resulting word is accepted
    H.assert $ atmtn `accepts` word

prop_missingD :: (Automaton a, Eq q) => a q SmallAlphabet -> Property
prop_missingD atmtn = property $ do
    -- generate one or more As or one B
    start <- forAll nfaStart
    -- generate zero or more Bs or Cs
    middle <- forAll $ Gen.list (Range.constant 0 100) $ Gen.element [B, C]
    -- construct and log the entire word
    let word = start <> middle
    annotateShow word
    -- check that the resulting word is not accepted
    H.assert $ not $ atmtn `accepts` word

prop_missingStart :: (Automaton a, Eq q) => a q SmallAlphabet -> Property
prop_missingStart atmtn = property $ do
    -- generate zero or more Cs or Ds
    middle <- forAll $ Gen.list (Range.constant 0 100) $ Gen.element [C, D]
    -- construct and log the entire word
    let word = middle <> [D]
    annotateShow word
    -- check that the resulting word is not accepted
    H.assert $ not $ atmtn `accepts` word

--------------------------------------------------------------------------------

nfa0Tests :: (Automaton a, Eq q) => String -> a q SmallAlphabet -> TestTree
nfa0Tests nm atmtn = testGroup nm
    [ testCase "rejects the empty word" $
        assertBool "The empty word is accepted" $ not $ atmtn `accepts` []
    , testProperty
        "accepts words as specified"
        "prop_nfaWords" $
        prop_nfaWords atmtn
    , testProperty
        "rejects words without a D at the end"
        "prop_missingD" $
        prop_missingD atmtn
    , testProperty
        "rejects words without the correct start"
        "prop_missingStart" $
        prop_missingStart atmtn
    ]

--------------------------------------------------------------------------------

prop_noDuplicates :: Property
prop_noDuplicates = property $ do
    xss <- forAll $ Gen.list (Range.constant 0 10)
                  $ Gen.list (Range.constant 0 20) Gen.binit
    unions xss === nub (unions xss)

prop_allElements :: Property
prop_allElements = property $ do
    xss <- forAll $ Gen.list (Range.constant 0 10)
                  $ Gen.list (Range.constant 0 20) Gen.alphaNum
    -- check that each unique symbol from the input list is present in the
    -- union of all the input lists
    H.assert $ all (`elem` unions xss) $ nub (concat xss)

unionsTests :: TestTree
unionsTests = testGroup "unions"
    [ testProperty
        "unions contain no duplicates"
        "prop_noDuplicates"
        prop_noDuplicates
    , testProperty
        "unions contains all elements from the input lists"
        "prop_allElements"
        prop_allElements
    ]

--------------------------------------------------------------------------------

prop_powersetSize :: Property
prop_powersetSize = property $ do
    xs <- forAll $ Gen.list (Range.constant 0 20) Gen.digit
    length (powerset xs) === 2^length xs

powersetTests :: TestTree
powersetTests = testGroup "powerset"
    [ testProperty
        "length (powerset xs) == 2^length xs"
        "prop_powersetSize"
        prop_powersetSize
    ]

--------------------------------------------------------------------------------

genBaseRegEx :: MonadGen m => m (RegEx Int)
genBaseRegEx = Gen.choice
    [ pure Empty
    , pure Epsilon
    , Symbol <$> Gen.int (Range.constant minBound maxBound)
    ]

prop_EmptyRegEx :: Property
prop_EmptyRegEx = property $ do
    word <- forAll $ Gen.list (Range.constant 0 100) $ Gen.element [A .. D]
    H.assert $ not $ toNFA Empty `accepts` word

prop_EpsilonRegEx :: Property
prop_EpsilonRegEx = property $ do
    word <- forAll $ Gen.list (Range.constant 1 100) $ Gen.element [A .. D]
    H.assert $ not $ toNFA Epsilon `accepts` word

prop_SymbolRegExEmpty :: Property
prop_SymbolRegExEmpty = property $ do
    x <- forAll $ Gen.int (Range.constant minBound maxBound)
    H.assert $ not $ toNFA (Symbol x) `accepts` []

prop_SymbolRegExAccept :: Property
prop_SymbolRegExAccept = property $ do
    x <- forAll $ Gen.int (Range.constant minBound maxBound)
    H.assert $ toNFA (Symbol x) `accepts` [x]

prop_SymbolRegExRejectSymbol :: Property
prop_SymbolRegExRejectSymbol = property $ do
    x <- forAll $ Gen.int (Range.constant 1 maxBound)
    H.assert $ not $ toNFA (Symbol 0) `accepts` [x]

prop_SymbolRegExRejectWord :: Property
prop_SymbolRegExRejectWord = property $ do
    x <- forAll $ Gen.int (Range.constant minBound maxBound)
    w <- forAll $ Gen.list (Range.constant 2 100)
                $ Gen.int (Range.constant minBound maxBound)
    H.assert $ not $ toNFA (Symbol x) `accepts` w

prop_PlusRegExAccept :: Property
prop_PlusRegExAccept = property $ do
    x <- forAll $ Gen.int (Range.constant minBound maxBound)
    y <- forAll $ Gen.int (Range.constant minBound maxBound)
    z <- forAll $ Gen.element [x,y]
    H.assert $ toNFA (Symbol x `Plus` Symbol y) `accepts` [z]

prop_PlusRegExReject :: Property
prop_PlusRegExReject = property $ do
    z <- forAll $ Gen.int (Range.constant 2 maxBound)
    H.assert $ not $ toNFA (Symbol 0 `Plus` Symbol 1) `accepts` [z]

prop_ConcatRegExAccept :: Property
prop_ConcatRegExAccept = property $ do
    x <- forAll $ Gen.int (Range.constant minBound maxBound)
    y <- forAll $ Gen.int (Range.constant minBound maxBound)
    H.assert $ toNFA (Symbol x `Concat` Symbol y) `accepts` [x,y]

prop_ConcatRegExReject :: Property
prop_ConcatRegExReject = property $ do
    x <- forAll $ Gen.int (Range.constant minBound maxBound)
    y <- forAll $ Gen.int (Range.constant minBound maxBound)
    w <- forAll $ Gen.choice
        [ pure []
        , pure [x]
        , pure [y]
        , Gen.list (Range.constant 3 100) (Gen.element [x,y])
        ]
    H.assert $ not $ toNFA (Symbol x `Concat` Symbol y) `accepts` w

prop_StarRegExEmpty :: Property
prop_StarRegExEmpty = property $ do
    regex <- forAll genBaseRegEx
    H.assert $ toNFA (Star regex) `accepts` []

prop_StarRegExRepeated :: Property
prop_StarRegExRepeated = property $ do
    x <- forAll $ Gen.int (Range.constant minBound maxBound)
    w <- forAll $ Gen.list (Range.constant 1 100) (pure x)
    H.assert $ toNFA (Star (Symbol x)) `accepts` w

regExTests :: TestTree
regExTests = testGroup "toNFA"
    [ testProperty
        "toNFA Empty accepts nothing"
        "prop_EmptyRegEx"
        prop_EmptyRegEx
    , testCase "toNFA Epsilon accepts the empty word" $
        assertBool "The empty word is not accepted" $
            toNFA @SmallAlphabet Epsilon `accepts` []
    , testProperty
        "toNFA Epsilon does not accept non-empty words"
        "prop_EpsilonRegEx"
        prop_EpsilonRegEx
    , testProperty
        "toNFA (Symbol x) rejects the empty word"
        "prop_SymbolRegExEmpty"
        prop_SymbolRegExEmpty
    , testProperty
        "toNFA (Symbol x) accepts [x]"
        "prop_SymbolRegExAccept"
        prop_SymbolRegExAccept
    , testProperty
        "toNFA (Symbol x) rejects other symbols"
        "prop_SymbolRegExRejectSymbol"
        prop_SymbolRegExRejectSymbol
    , testProperty
        "toNFA (Symbol x) rejects longer words"
        "prop_SymbolRegExRejectWord"
        prop_SymbolRegExRejectWord
    , testProperty
        "toNFA (Plus l r) accepts one or the other"
        "prop_PlusRegExAccept"
        prop_PlusRegExAccept
    , testProperty
        "toNFA (Plus l r) rejects other inputs"
        "prop_PlusRegExReject"
        prop_PlusRegExReject
    , testProperty
        "toNFA (Concat l r) accepts one after the other"
        "prop_ConcatRegExAccept"
        prop_ConcatRegExAccept
    , testProperty
        "toNFA (Concat l r) does not accept other inputs"
        "prop_ConcatRegExReject"
        prop_ConcatRegExReject
    , testProperty
        "toNFA (Star e) accepts the empty word"
        "prop_StarRegExEmpty"
        prop_StarRegExEmpty
    , testProperty
        "toNFA (Star e) accepts repeated input"
        "prop_StarRegExRepeated"
        prop_StarRegExRepeated
    , nfa0Tests "toNFA regExp0" (toNFA regExp0)
    ]

--------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Formal languages"
    [ dfa0Tests
    , nfa0Tests "nfa0" nfa0
    , unionsTests
    , powersetTests
    , after AllSucceed "dfa0"
    $ after AllSucceed "nfa0"
    $ after AllSucceed "unions"
    $ after AllSucceed "powerset"
    $ nfa0Tests "nfa2dfa" (nfa2dfa nfa0)
    , regExTests
    ]

ingredients :: [Ingredient]
ingredients = [antXMLRunner, listingTests, consoleTestReporter]

-- | The main entry point to the test suite.
main :: IO ()
main = defaultMainWithIngredients ingredients tests

--------------------------------------------------------------------------------
