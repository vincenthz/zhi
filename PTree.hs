module PTree
	(
	-- * Data Types
	  PTree
	-- * indexing
	, indexerToString
	, indexerFromString
	-- * manipulation function
	, empty
	, insert
	, lookup
	, lookupPrefix1
	) where

import Data.Either
import Data.Word
import Data.Vector ((!), (//), Vector)
import qualified Data.Vector as V
import Prelude hiding (lookup)

-- | underlaying data type for a patricia tree.
data PTree a = PTree {-# UNPACK #-} !(Vector (Maybe (PTree a))) !(Maybe a)

-- | number of children trees on a parent.
nbPtree :: Int
nbPtree = length (['0'..'9'] ++ ['a'..'z'])

indexerToString :: [Int] -> String
indexerToString = map itoc where
	itoc i
		| i >= 0 && i <= 9 = toEnum (fromEnum '0' + i)
		| otherwise        = toEnum (fromEnum 'a' + (i - 10))

indexerFromString :: String -> Either String [Int]
indexerFromString s
	| lefts is /= [] = Left $ head $ lefts is
	| otherwise      = Right $ rights is
	where
		-- | only allow [0-9], [a-z] and [A-Z] for indexing.
		is = map ctoi s
		ctoi c
			| c >= 'a' && c <= 'z' = Right (fromEnum c - fromEnum 'a' + 10)
			| c >= 'A' && c <= 'Z' = Right (fromEnum c - fromEnum 'A' + 10)
			| c >= '0' && c <= '9' = Right (fromEnum c - fromEnum '0')
			| otherwise            = Left ("bad format for indexer: character " ++ show c ++ " in " ++ show s)

-- | empty patricia tree.
empty a = PTree (V.replicate nbPtree Nothing) a

-- | insert node in a patricia tree and returns the new root.
insert :: [Int] -> a -> PTree a -> PTree a
insert []     a (PTree children _)   = PTree children (Just a)
insert (x:xs) a (PTree children val) = case children ! x of
	Nothing     -> PTree (children // [(x, Just $ insert xs a (empty Nothing))]) val 
	Just child  -> PTree (children // [(x, Just $ insert xs a child)]) val

-- | lookup node in a ptree
lookup :: [Int] -> PTree a -> Maybe a
lookup []     (PTree _ val)      = val
lookup (x:xs) (PTree children _) = case children ! x of
	Nothing    -> Nothing
	Just child -> lookup xs child

lookupPrefix1 :: [Int] -> PTree a -> (Maybe a, [(Char, Maybe a)])
lookupPrefix1 [] (PTree children val)   = (val, enumChild children)
	where
		enumChild v = V.ifoldl accChild [] v
		accChild acc _ Nothing            = acc
		accChild acc i (Just (PTree _ v)) = (head $ indexerToString [i],v) : acc

-- | lookup node in a ptree and also returns 1 level of children if any.
lookupPrefix1 (x:xs) (PTree children _) = case children ! x of
	Nothing    -> (Nothing, [])
	Just child -> lookupPrefix1 xs child
