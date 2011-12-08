{-# LANGUAGE OverloadedStrings #-}
module CEDict
	( Dict
	, Entry(..)
	, readDict
	) where

import qualified Data.ByteString.Lazy as L
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T

import Control.Applicative

data Entry = Entry
	{ simplified  :: Maybe Text
	, traditional :: Text
	, pinyin      :: Text
	, meanings    :: [Text]
	} deriving (Show,Eq)

type Dict = [Entry]

readDict filename =  map format . filter (not . isOther) . T.lines . T.decodeUtf8 <$> L.readFile filename
	where
		isOther line = "#" `T.isPrefixOf` line || T.null line
		format :: Text -> Entry
		format line =
			let (keyRaw:meanings) = T.splitOn "/" $ T.init $ T.dropWhileEnd (== '\r') line in
			let (traditional:simplified:remain) = T.splitOn " " keyRaw in
			let pinyin = T.init $ T.tail $ T.concat remain in
			Entry { simplified = if simplified == traditional then Nothing else Just simplified, traditional = traditional, pinyin = pinyin, meanings = meanings }
