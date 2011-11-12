module Regex where

import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

type Groups = Map Int String
type Matcher = Data -> [Data]

data Data = Data {original :: String, remainder :: String, groups :: Groups}

mPredicate :: (Char -> Bool) -> Matcher
mPredicate p = \d -> case remainder d of (c:cs) | p c -> [d {remainder = cs}]; _ -> []

mAny :: Matcher
mAny = mPredicate $ const True

mChar :: Char -> Matcher
mChar = mPredicate . (==)

mOr :: [Matcher] -> Matcher
mOr ms = \d -> ms >>= ($d)

mSeq :: [Matcher] -> Matcher
mSeq ms = \d -> foldl (>>=) [d] ms

mString :: String -> Matcher
mString = mSeq . map mChar

mLazy :: Matcher -> Matcher
mLazy = (reverse .)

mOptional :: Matcher -> Matcher
mOptional = mChains . (:[])

mZeroOrMore :: Matcher -> Matcher
mZeroOrMore = mChains . repeat

mOneOrMore :: Matcher -> Matcher
mOneOrMore m = init . mZeroOrMore m

mCount :: Int -> Int -> Matcher -> Matcher
mCount a b m = mSeq $ replicate a m ++ [mChains $ replicate (b - a) m]

mGroup :: Int -> Matcher -> Matcher
mGroup n m = \a -> map (\b -> withGroups b (Map.singleton n (diff a b))) $ m a
    where diff (Data _ a _) (Data _ b _) = take (length a - length b) a

mRef :: Int -> Matcher
mRef n = \d -> maybe (const []) mString (Map.lookup n $ groups d) d

mStart :: Matcher
mStart = filter (\(Data o r _) -> o == r) . (:[])

mEnd :: Matcher
mEnd = filter (null . remainder) . (:[])

mAhead :: Matcher -> Matcher
mAhead = mOnlyGroups

mBehind :: Matcher -> Matcher
mBehind m = mOnlyGroups $ filter (null . remainder) . (>>= m) . prefixes
    where prefixes (Data o r gs) = map (\s -> Data s s gs) $ tails $ take (length o - length r) o

mNot :: Matcher -> Matcher
mNot m = filter (null . m) . (:[])

withGroups :: Data -> Groups -> Data
withGroups d gs = d{groups = Map.union gs $ groups d}

mChains :: [Matcher] -> Matcher
mChains ms = \d -> reverse . (>>= id) $ takeWhile (not . null) . scanl (>>=) [d] $ ms

mOnlyGroups :: Matcher -> Matcher
mOnlyGroups m = \d -> map (withGroups d) $ nub $ map groups $ m d

data MatchResult = MatchResult Bool Groups deriving (Show, Eq)

match :: Matcher -> String -> MatchResult
match m s = MatchResult (isJust d) (maybe Map.empty groups d)
    where d = find (null . remainder) $ m $ Data s s Map.empty