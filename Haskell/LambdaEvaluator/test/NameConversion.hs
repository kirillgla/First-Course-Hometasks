module NameConversion where

import           Data.Maybe
import qualified DeBruijnLambdas      as Unnamed
import           NameConversionData
import qualified NamedLambdas         as Named
import           NamedLambdasInternal
import           Test.Tasty
import           Test.Tasty.HUnit

nameConversionTests :: TestTree
nameConversionTests = testGroup "Name conversion tests"
  [ testFindAnyName
  , testFindSomeName
  , testUnnameLoop
  , testUnnameTrue
  , testUnnameFalse
  , testUnnameIf
  , testReNameLoop
  , testReNameTrue
  , testReNameFalse
  , testReNameIf
  ]

-- find new name tests

testFindAnyName :: TestTree
testFindAnyName = testCase "Find any name" $
  assertEqual "" "v0" (findNewName [])

testFindSomeName :: TestTree
testFindSomeName = testCase "Find some name" $
  assertEqual "" "v3" (findNewName ["v2", "v1", "v0"])

-- unname tests

testUnnameLoop :: TestTree
testUnnameLoop = testUnname "Unname loop" namedLoop unnamedLoop

testUnnameTrue :: TestTree
testUnnameTrue = testUnname "Unname true lambda" namedTrueLambda unnamedTrueLambda

testUnnameFalse :: TestTree
testUnnameFalse = testUnname "Unname false lambda" namedFalseLambda unnamedFalseLambda

testUnnameIf :: TestTree
testUnnameIf = testUnname "Unname if lambda" namedIfLambda unnamedIfLambda

-- re-name tests

testReNameLoop :: TestTree
testReNameLoop = testReName "Re-name loop" unnamedLoop

testReNameTrue :: TestTree
testReNameTrue = testReName "Re-name true lambda" unnamedTrueLambda

testReNameFalse :: TestTree
testReNameFalse = testReName "Re-name false lambda" unnamedFalseLambda

testReNameIf :: TestTree
testReNameIf = testReName "Re-name if lambda" unnamedIfLambda

-- utils

testUnname :: Eq i => String -> Named.Lambda i -> Unnamed.Lambda i -> TestTree
testUnname name named unnamed = testCase name $
  assertEqual "" (fromJust $ Named.toUnnamedLambda named) unnamed

testReName :: Eq i => String -> Unnamed.Lambda i -> TestTree
testReName name unnamed = testUnname name named unnamed where
  named = fromJust $ Named.toNamedLambda unnamed
