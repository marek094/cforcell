{- written by MarekCerny.com -}
{-# LANGUAGE ViewPatterns, PatternSynonyms  #-}
{-# LANGUAGE TupleSections                  #-}
{-# LANGUAGE UnicodeSyntax                  #-} 
{-# LANGUAGE LambdaCase, MultiWayIf         #-}
import Data.Tuple
import Data.Bits
import Data.Maybe
import Data.Function
import Data.List
import Control.Exception
import qualified Data.HashMap.Strict as M
import Data.Sequence (Seq, (|>), (<|), (><))
import qualified Data.Sequence as S

main = interact $ unlines . sol . allInt . lines

sol :: [[Int]] -> [String]
sol as = wrap $ show $ soly 42 $ zip [1..] $ head as

soly :: Int → [(Int,Int)] → Int
soly a as
  | Just x ← lookup 10 as = x
  | Just x ← lookup 20 as = x+11
  | otherwise           = a


--------------------------------------------------------------------------------
groupOn :: Int -> [a] -> [[a]]
groupOn _ [] = []
groupOn n xs = let (sts, nds) = splitAt n xs in sts : groupOn n nds

fromBool :: Num a => Bool -> a
fromBool b = if b then 1 else 0

allInt :: [String] -> [[Int]]
allInt = map ((map read) . words)

swrt :: Ord a => (a, a) -> (a, a)
swrt (a, b) | compare b a == LT = (b, a) | otherwise = (a, b)

wrap :: a -> [a]
wrap x = [x]

toSnd :: a->b-> (a,b)
toSnd x y = (x,y)

toFst :: b->a-> (a,b)
toFst y x = (x,y)

addCnt :: [String] -> [String]
addCnt ls = (show $ length ls) : ls

toBitMap :: Int -> [(Int, Bool)]
toBitMap = toBitMap' 31
  where
    toBitMap' (-1) _ = []
    toBitMap' n v = (n, v .&. (1`shiftL`n) > 0) : toBitMap' (n-1) v
--------------------------------------------------------------------------------
pattern Empty   <- (S.viewl -> S.EmptyL)
pattern x :< xs <- (S.viewl -> x S.:< xs)
pattern xs :> x <- (S.viewr -> xs S.:> x)
--------------------------------------------------------------------------------
