module Chapter03.Spec where

import Test.Tasty
import Test.Tasty.QuickCheck

import Data.List (sort)

import Chapter03.SortsSpec as SortsSpec
import Chapter03.SearchesSpec as SearchesSpec

test_Chapter3Sort :: TestTree
test_Chapter3Sort = testGroup "Chapter 3" [SortsSpec.tests, SearchesSpec.tests]