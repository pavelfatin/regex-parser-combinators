module Regex_Test where

import Data.List
import qualified Data.Map as Map
import Test.HUnit
import Regex

testAny = TestCase $ do
    assertVariants "." (mAny) "" []
    assertVariants "." (mAny) "a" ["a"]
    assertVariants "." (mAny) "ab" ["a"]

testChar = TestCase $ do
    assertVariants "a" (mChar 'a') "" []
    assertVariants "a" (mChar 'a') "b" []
    assertVariants "a" (mChar 'a') "a" ["a"]
    assertVariants "a" (mChar 'a') "ab" ["a"]
    assertVariants "a" (mChar 'a') "aa" ["a"]

testSeq = TestCase $ do
    assertVariants "ab" (mSeq [mChar 'a', mChar 'b']) "" []
    assertVariants "ab" (mSeq [mChar 'a', mChar 'b']) "c" []
    assertVariants "ab" (mSeq [mChar 'a', mChar 'b']) "b" []
    assertVariants "ab" (mSeq [mChar 'a', mChar 'b']) "a" []
    assertVariants "ab" (mSeq [mChar 'a', mChar 'b']) "ac" []
    assertVariants "ab" (mSeq [mChar 'a', mChar 'b']) "aa" []
    assertVariants "ab" (mSeq [mChar 'a', mChar 'b']) "bb" []
    assertVariants "ab" (mSeq [mChar 'a', mChar 'b']) "ab" ["ab"]
    assertVariants "ab" (mSeq [mChar 'a', mChar 'b']) "abc" ["ab"]
    assertVariants "abc" (mSeq [mChar 'a', mChar 'b', mChar 'c']) "abc" ["abc"]

testString = TestCase $ do
    assertVariants "" (mString "") "" [""]
    assertVariants "" (mString "") "bar" [""]
    assertVariants "f" (mString "f") "" []
    assertVariants "f" (mString "f") "b" []
    assertVariants "f" (mString "f") "f" ["f"]
    assertVariants "foo" (mString "foo") "bar" []
    assertVariants "foo" (mString "foo") "fo" []
    assertVariants "foo" (mString "foo") "foo" ["foo"]
    assertVariants "foo" (mString "foo") "foobar" ["foo"]

testOr = TestCase $ do
    assertVariants "a|b" (mOr [mChar 'a', mChar 'b']) "" []
    assertVariants "a|b" (mOr [mChar 'a', mChar 'b']) "c" []
    assertVariants "a|b" (mOr [mChar 'a', mChar 'b']) "a" ["a"]
    assertVariants "a|b" (mOr [mChar 'a', mChar 'b']) "b" ["b"]
    assertVariants "a|b" (mOr [mChar 'a', mChar 'b']) "ab" ["a"]
    assertVariants "a|b" (mOr [mChar 'a', mChar 'b']) "ba" ["b"]
    assertVariants "a|b|c" (mOr [mChar 'a', mChar 'b', mChar 'c']) "c" ["c"]

testOptional = TestCase $ do
    assertVariants "a?" (mOptional $ mChar 'a') "" [""]
    assertVariants "a?" (mOptional $ mChar 'a') "b" [""]
    assertVariants "a?" (mOptional $ mChar 'a') "a" ["a", ""]

testZeroOrMore = TestCase $ do
    assertVariants "a*" (mZeroOrMore $ mChar 'a') "" [""]
    assertVariants "a*" (mZeroOrMore $ mChar 'a') "b" [""]
    assertVariants "a*" (mZeroOrMore $ mChar 'a') "a" ["a", ""]
    assertVariants "a*" (mZeroOrMore $ mChar 'a') "aa" ["aa", "a", ""]
    assertVariants "a*" (mZeroOrMore $ mChar 'a') "aaa" ["aaa", "aa", "a", ""]

testOneOrMore = TestCase $ do
    assertVariants "a+" (mOneOrMore $ mChar 'a') "" []
    assertVariants "a+" (mOneOrMore $ mChar 'a') "b" []
    assertVariants "a+" (mOneOrMore $ mChar 'a') "a" ["a"]
    assertVariants "a+" (mOneOrMore $ mChar 'a') "aa" ["aa", "a"]
    assertVariants "a+" (mOneOrMore $ mChar 'a') "aaa" ["aaa", "aa", "a"]

testLazy = TestCase $ do
    assertVariants "a+?" (mLazy $ mOneOrMore $ mChar 'a') "" []
    assertVariants "a+?" (mLazy $ mOneOrMore $ mChar 'a') "aa" ["a", "aa"]

testCount = TestCase $ do
    assertVariants "a{2,5}" (mCount 2 5 $ mChar 'a') "" []
    assertVariants "a{2,5}" (mCount 2 5 $ mChar 'a') "a" []
    assertVariants "a{2,5}" (mCount 2 5 $ mChar 'a') "aa" ["aa"]
    assertVariants "a{2,5}" (mCount 2 5 $ mChar 'a') "aaa" ["aaa", "aa"]
    assertVariants "a{2,5}" (mCount 2 5 $ mChar 'a') "aaaa" ["aaaa", "aaa", "aa"]
    assertVariants "a{2,5}" (mCount 2 5 $ mChar 'a') "aaaaa" ["aaaaa", "aaaa", "aaa", "aa"]
    assertVariants "a{2,5}" (mCount 2 5 $ mChar 'a') "aaaaaa" ["aaaaa", "aaaa", "aaa", "aa"]

testGroup = TestCase $ do
    assertVariants "(a)" (mGroup 1 $ mChar 'a') "" []
    assertVariants "(a)" (mGroup 1 $ mChar 'a') "b" []
    assertVariants "(a)" (mGroup 1 $ mChar 'a') "a" ["a(1:a)"]
    assertVariants "(a*)" (mGroup 1 $ mZeroOrMore $ mChar 'a') "aa" ["aa(1:aa)", "a(1:a)", "(1:)"]
    assertVariants "(a)b" (mSeq [mGroup 1 $ mChar 'a', mChar 'b']) "a" []
    assertVariants "(a)b" (mSeq [mGroup 1 $ mChar 'a', mChar 'b']) "ab" ["ab(1:a)"]
    assertVariants "a(b)" (mSeq [mChar 'a', mGroup 1 $ mChar 'b']) "a" []
    assertVariants "a(b)" (mSeq [mChar 'a', mGroup 1 $ mChar 'b']) "ab" ["ab(1:b)"]
    assertVariants "(a)(b)" (mSeq [mGroup 1 $ mChar 'a', mGroup 2 $ mChar 'b']) "ab" ["ab(1:a, 2:b)"]
    assertVariants "((a)b)" (mSeq [mGroup 1 $ mSeq [mGroup 2 $ mChar 'a', mChar 'b']]) "ab" ["ab(1:ab, 2:a)"]
    assertVariants "(?:(a)|(b))" (mOr [mGroup 1 $ mChar 'a', mGroup 2 $ mChar 'b']) "a" ["a(1:a)"]
    assertVariants "(?:(a)|(b))" (mOr [mGroup 1 $ mChar 'a', mGroup 2 $ mChar 'b']) "b" ["b(2:b)"]

testRef = TestCase $ do
    assertVariants "(.)\\1" (mSeq [mGroup 1 $ mAny, mRef 1]) "" []
    assertVariants "(.)\\1" (mSeq [mGroup 1 $ mAny, mRef 1]) "a" []
    assertVariants "(.)\\1" (mSeq [mGroup 1 $ mAny, mRef 1]) "ab" []
    assertVariants "(.)\\1" (mSeq [mGroup 1 $ mAny, mRef 1]) "aa" ["aa(1:a)"]
    assertVariants "((.)\\2)\\1" (mSeq [mGroup 1 $ mSeq [mGroup 2 $ mAny, mRef 2], mRef 1]) "aaaa" ["aaaa(1:aa, 2:a)"]
    assertVariants "(.?)\\1" (mSeq [mGroup 1 $ mOptional $ mAny, mRef 1]) "" ["(1:)"]
    assertVariants "\\1" (mRef 1) "" []
    assertVariants "(?:(a)|(b))\1" (mSeq [mOr [mGroup 1 $ mChar 'a', mGroup 2 $ mChar 'b'], mRef 1]) "ab" []
    assertVariants "(?:(a)|(b))\1" (mSeq [mOr [mGroup 1 $ mChar 'a', mGroup 2 $ mChar 'b'], mRef 1]) "aa" ["aa(1:a)"]
    assertVariants "(?:(a)|(b))\2" (mSeq [mOr [mGroup 1 $ mChar 'a', mGroup 2 $ mChar 'b'], mRef 2]) "ab" []
    assertVariants "(?:(a)|(b))\2" (mSeq [mOr [mGroup 1 $ mChar 'a', mGroup 2 $ mChar 'b'], mRef 2]) "bb" ["bb(2:b)"]

testStart = TestCase $ do
    assertVariants "^a" (mSeq [mStart, mChar 'a']) "a" ["a"]
    assertVariants "a^" (mSeq [mChar 'a', mStart]) "a" []

testEnd = TestCase $ do
    assertVariants "a$" (mSeq [mChar 'a', mEnd]) "a" ["a"]
    assertVariants "$a" (mSeq [mEnd, mChar 'a']) "a" []

testAhead = TestCase $ do
    assertVariants "a(?=b)b" (mSeq [mChar 'a', mAhead $ mChar 'b', mChar 'b']) "ab" ["ab"]
    assertVariants "a(?=a)b" (mSeq [mChar 'a', mAhead $ mChar 'a', mChar 'b']) "ab" []
    assertVariants "(.)(?=\\1)\\1" (mSeq [mGroup 1 $ mAny, mAhead $ mRef 1, mRef 1]) "aa" ["aa(1:a)"]
    assertVariants "(?=(.)).\\1" (mSeq [mAhead $ mGroup 1 $ mAny, mAny, mRef 1]) "aa" ["aa(1:a)"]
    assertVariants "(?=.{1, 2})ab" (mSeq [mAhead $ mCount 1 2 $ mAny, mString "ab"]) "ab" ["ab"]

testBehind = TestCase $ do
    assertVariants "a(?<=a)b" (mSeq [mChar 'a', mBehind $ mChar 'a', mChar 'b']) "ab" ["ab"]
    assertVariants "a(?<=b)b" (mSeq [mChar 'a', mBehind $ mChar 'b', mChar 'b']) "ab" []
    assertVariants "a(?<=aa)b" (mSeq [mChar 'a', mBehind $ mString "aa", mChar 'b']) "ab" []
    assertVariants "a(?<=ab)b" (mSeq [mChar 'a', mBehind $ mString "ab", mChar 'b']) "ab" []
    assertVariants "caa(?<=a{2,3})b" (mSeq [mString "caa", mBehind (mCount 2 3 $ mChar 'a'), mChar 'b']) "caab" ["caab"]
    assertVariants "caa(?<=a{3,4})a" (mSeq [mString "caa", mBehind (mCount 3 4 $ mChar 'a'), mChar 'a']) "caaa" []
    assertVariants "(.)(?<=\\1)\\1" (mSeq [mGroup 1 $ mAny, mBehind $ mRef 1, mRef 1]) "aa" ["aa(1:a)"]
    assertVariants ".(?<=(.))\\1" (mSeq [mAny, mBehind $ mGroup 1 $ mAny, mRef 1]) "aa" ["aa(1:a)"]
    assertVariants "ab(?<=.{1, 2})" (mSeq [mString "ab", mBehind $ mCount 1 2 $ mAny]) "ab" ["ab"]

testNot = TestCase $ do
    assertVariants "a(?!b)b" (mSeq [mChar 'a', mNot $ mAhead $ mChar 'b', mChar 'b']) "ab" []
    assertVariants "a(?!a)b" (mSeq [mChar 'a', mNot $ mAhead $ mChar 'a', mChar 'b']) "ab" ["ab"]
    assertVariants "(.)(?!\\1)\\1" (mSeq [mGroup 1 $ mAny, mNot $ mAhead $ mRef 1, mRef 1]) "aa" []

testMatch = TestCase $ do
    assertEqual "(a+)a -> a" (MatchResult False Map.empty) (match m "a")
    assertEqual "(a+)a -> aaa" (MatchResult True (Map.fromList [(1, "aa")])) (match m "aaa")
    where m = mSeq [mGroup 1 $ (mOneOrMore $ mChar 'a'), mChar 'a']

assertVariants :: String -> Matcher -> String -> [String] -> Assertion
assertVariants name m s vs = assertEqual (name ++ " -> " ++ s) vs $ map format (m $ Data s s Map.empty)

format (Data o r gs) = case Map.toList gs of
    [] -> text
    gs -> text ++ "(" ++ (intercalate ", " $ map (\(n, s) -> show n ++ ':':s) gs) ++ ")"
    where text = take (length o - length r) o

tests = TestList [TestLabel "testAny" testAny,
                  TestLabel "testChar" testChar,
                  TestLabel "testSeq" testSeq,
                  TestLabel "testString" testString,
                  TestLabel "testOr" testOr,
                  TestLabel "testOptional" testOptional,
                  TestLabel "testZeroOrMore" testZeroOrMore,
                  TestLabel "testOneOrMore" testOneOrMore,
                  TestLabel "testLazy" testLazy,
                  TestLabel "testCount" testCount,
                  TestLabel "testGroup" testGroup,
                  TestLabel "testRef" testRef,
                  TestLabel "testStart" testStart,
                  TestLabel "testEnd" testEnd,
                  TestLabel "testAhead" testAhead,
                  TestLabel "testBehind" testBehind,
                  TestLabel "testNot" testNot,
                  TestLabel "testMatch" testMatch]

main = runTestTT tests