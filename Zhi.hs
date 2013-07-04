{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
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
import qualified Data.Map as M
import CEDict

readInputFileToTree file = makeTree <$> readInputFile file where
	makeTree = foldl' (\acc (k,v) -> PTree.insert k v acc) (PTree.empty Nothing)

readInputFile :: FilePath -> IO [([Int],[String])]
readInputFile file = rights . map splitLine . T.lines . decodeUtf8 <$> L.readFile file where
	splitLine line = case map T.unpack $ T.split (== ' ') line of
		[]       -> error "wrong format"
		key:[]   -> error "wrong format"
		key:vals -> case PTree.indexerFromString key of
			Left err    -> Left ("line format wrong : " ++ show err)
			Right index -> Right (index, vals)

romanizations = ["jyutping", "yale", "pinyin"]

toTree :: [([Int],[String])] -> M.Map String String
toTree l = foldl app M.empty l where
	app :: M.Map String String -> ([Int], [String]) -> M.Map String String
	app tree (key, vals) = foldl app2 tree vals where
		app2 t v = M.insert v k t
		k        = PTree.indexerToString key

yaleRomTree :: IO (M.Map String String)
yaleRomTree     = zhiRomanizationPath "yale" >>= \f -> (toTree <$> readInputFile f)
jyutpingRomTree = zhiRomanizationPath "jyutping" >>= \f -> (toTree <$> readInputFile f)
pinyinRomTree   = zhiRomanizationPath "pinyin" >>= \f -> (toTree <$> readInputFile f)

pinyinPrefixTree   = zhiRomanizationPath "pinyin" >>= readInputFileToTree
juytpingPrefixTree = zhiRomanizationPath "juytping" >>= readInputFileToTree
yalePrefixTree     = zhiRomanizationPath "yale" >>= readInputFileToTree

searchAndPrint :: Bool -> PTree.PTree [String] -> String -> IO ()
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

zhiPathDir = (++ "/.config/zhi/") `fmap` getEnv "HOME"
zhiRomanizationPath tableName = (++ ("table-" ++ tableName)) `fmap` zhiPathDir
zhiDictionaryPath = (++ "dict_ce") `fmap` zhiPathDir

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
processArgs cfg (x:xs)              = processArgs (cfg { others = unicodiseString x : others cfg }) xs
	where unicodiseString = T.unpack . decodeUtf8 . LC.pack 

revLists cfg = cfg { others = reverse $ others cfg }

action cfg@(others -> "dict":arg:[]) = do
	yale     <- yaleRomTree
	jyutping <- jyutpingRomTree
	pinyin   <- pinyinRomTree

	dictFile <- zhiDictionaryPath

	exist <- doesFileExist dictFile
	unless exist $ error "dictionary doesn't exists"

	let name = T.pack arg
	ents <- filter (\ent -> simplified ent == Just name || traditional ent == name) <$> readDict dictFile
	when (null ents) $ putStrLn (arg ++ " not found in dictionary")
	mapM_ (showEnt (yale,jyutping,pinyin)) ents
	where showEnt (yale,jyutping,pinyin) found = do
		printf "===== %s %s=====\n" (T.unpack $ traditional found) (maybe "" ((++ " ") . T.unpack) $ simplified found)
		mapM_ (putStrLn . (" * " ++) . T.unpack) $ meanings found
		let getString t = do
			let r = M.lookup (T.unpack $ traditional found) t
			let r2 = maybe Nothing (\s -> M.lookup (T.unpack s) t) (simplified found)
			maybe "not found" id (r `mplus` r2)
		printf "juytping: %s\n" (getString jyutping)
		printf "yale    : %s\n" (getString yale)
		printf "pinyin  : %s\n" (getString pinyin)

action cfg@(others -> args) = do
	doRomanizationResolve cfg

doRomanizationResolve cfg = do
	dictFile <- zhiRomanizationPath (table cfg)

	exist <- doesFileExist dictFile
	unless exist $ error "romanization table requested doesn't exists"

	tree <- readInputFileToTree dictFile

	mapM_ (searchAndPrint (descendant cfg) tree) (others cfg)

main = do
	cfg <- revLists . processArgs defaultConfig <$> getArgs
	--mapM_ putStrLn $ others cfg
	action cfg
