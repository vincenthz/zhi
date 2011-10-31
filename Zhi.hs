module Main where

import qualified Data.ByteString.Lazy as L
import Data.Text.Lazy.Encoding
import qualified Data.Text.Lazy as T

import Control.Applicative ((<$>))
import Control.Monad
import Data.List (foldl')

import Text.Printf
import Data.Either

import System.IO
import System.Directory
import System.Environment

import qualified PTree

readInputFile file = makeTree . rights . map splitLine . T.lines . decodeUtf8 <$> L.readFile file
	where
		splitLine line = case map T.unpack $ T.split (== ' ') line of
			[]       -> error "wrong format"
			key:[]   -> error "wrong format"
			key:vals -> case PTree.indexerFromString key of
				Left err    -> Left ("line format wrong : " ++ show err)
				Right index -> Right (index, vals)
		makeTree = foldl' (\acc (k,v) -> PTree.insert k v acc) (PTree.empty Nothing)

searchAndPrint printDescendant tree arg = do
	let indexer = either error id $ PTree.indexerFromString arg
	let (p', c) = PTree.lookupPrefix1 indexer tree
	printf "%s: " arg
	case p' of
		Nothing -> printf "not found "
		Just p  -> mapM_ (printf "%s ") p
	when (printDescendant) $ do
		printf "("
		mapM_ (printf "%c ") $ map fst c
		printf ")"
	printf "\n"

data Config = Config
	{ table      :: String
	, descendant :: Bool
	, others     :: [String]
	} deriving (Show,Eq)

defaultConfig = Config
	{ table  = "jyutping"
	, descendant = False
	, others = []
	}

processArgs cfg []                  = cfg
processArgs cfg ("--table":dict:xs) = processArgs (cfg { table = dict }) xs
processArgs cfg ("-t":dict:xs)      = processArgs (cfg { table = dict }) xs
processArgs cfg ("-d":xs)           = processArgs (cfg { descendant = True }) xs
processArgs cfg (x:xs)              = processArgs (cfg { others = x : others cfg }) xs

main = do
	cfg  <- processArgs defaultConfig <$> getArgs
	home <- getEnv "HOME"
	let dictFile = home ++ "/.config/zhi/table-" ++ table cfg

	exist <- doesFileExist dictFile
	unless exist $ error "table requested doesn't exists"

	tree <- readInputFile dictFile

	mapM_ (searchAndPrint (descendant cfg) tree) (reverse $ others cfg)
