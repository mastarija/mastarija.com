{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
--
module Mastarija.Domain where
--
import Data.Char ( isDigit )
import Data.Text ( Text , pack , unpack )
import Data.Time ( Day , parseTimeM , defaultTimeLocale , toGregorian )
import Data.Maybe ( listToMaybe )
import Data.Aeson ( ToJSON (..) , FromJSON (..) , withText , withObject , (.:) , (.:?) )
--
import GHC.Generics ( Generic )
import Numeric.Natural ( Natural )
--
import Control.Monad ( join )
import Control.Monad.Trans.Maybe ( MaybeT , runMaybeT )
--

newtype Tag = Tag
  { unTag :: Text
  } deriving ( Show )

newtype Link = Link
  { unLink :: FilePath
  } deriving ( Show )

newtype Slug = Slug
  { unSlug :: Text
  } deriving ( Show )

newtype Path = Path
  { unPath :: FilePath
  } deriving ( Show )

data Date = Date
  { y :: Natural
  , m :: Natural
  , d :: Natural
  } deriving ( Eq , Show , Generic , ToJSON )

instance Ord Date where
  compare a b
    | y a /= y b = compare ( y a ) ( y b )
    | m a /= m b = compare ( m a ) ( m b )
    | d a /= d b = compare ( d a ) ( d b )

data Meta = Meta
  { title :: Text
  , caption :: Maybe Text
  , excerpt :: Maybe Text
  , taglist :: [ Tag ]
  , pubDate :: Date
  , modDate :: Maybe Date
  } deriving ( Show , Generic , ToJSON )

data Article = Article
  { meta :: Meta
  , content :: Maybe Text
  } deriving ( Show , Generic , ToJSON )

data ArticlePage = ArticlePage
  { link :: Link
  , slug :: Slug
  , source :: Path
  , article :: Article
  , children :: [ ArticlePage ]
  , hasChildren :: Bool
  } deriving ( Show , Generic , ToJSON )

data Website = Website
  { name :: Text
  , page :: ArticlePage
  } deriving ( Show , Generic , ToJSON )

data TheError
  = InvalidMD Path
  | InvalidYL Path
  | MissingFile Path
  | MissingDirectory Path
  deriving ( Show )


--

instance ToJSON Tag where
  toJSON ( Tag t ) = toJSON t

instance FromJSON Tag where
  parseJSON = withText "Tag" $ pure . Tag

instance ToJSON Link where
  toJSON ( Link l ) = toJSON l

instance FromJSON Link where
  parseJSON = withText "Link" $ pure . Link . unpack

instance ToJSON Slug where
  toJSON ( Slug s ) = toJSON s

instance FromJSON Slug where
  parseJSON = withText "Slug" $ pure . Slug

instance ToJSON Path where
  toJSON ( Path p ) = toJSON p

instance FromJSON Path where
  parseJSON = withText "Path" $ pure . Path . unpack


instance FromJSON Meta where
  parseJSON = withObject "Article" $ \ o -> do
    mtitle   <- o .:? "title"
    mcaption <- o .:? "caption"
    mexcerpt <- o .:? "excerpt"
    mtaglist <- o .:? "taglist"
    mpubDate <- o .:? "pubDate"
    mmodDate <- o .:? "modDate"

    title   <- maybe ( fail "missing_title" ) pure mtitle
    caption <- pure mcaption
    excerpt <- pure mexcerpt
    taglist <- pure $ maybe mempty id mtaglist
    pubDate <- maybe ( fail "invalid_pubDate" ) pure
               $ maybe ( fail "missing_pubDate" ) parseDate mpubDate
    modDate <- case mmodDate of
                Nothing -> pure Nothing
                Just md -> maybe ( fail "invalid_modDate" ) ( pure . Just ) $ parseDate md

    pure Meta {..}

    where
      parseDate :: String -> Maybe Date
      parseDate s = do
        t <- ( parseTimeM True defaultTimeLocale "%0Y-%0m-%0d" s )
        ( gy , gm , gd ) <- pure $ toGregorian t
        pure Date
          { y = fromIntegral gy
          , m = fromIntegral gm
          , d = fromIntegral gd
          }
