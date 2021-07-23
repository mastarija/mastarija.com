{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
--
module Mastarija.Data where
--
import Data.Maybe ( listToMaybe )
import Data.Text ( unpack )
import Data.Char ( isDigit )
import Data.Aeson ( ToJSON (..) , FromJSON (..) , Value (..) , withObject , (.:) )
import Data.Aeson.Types ( prependFailure , typeMismatch )
import Text.Read ( readMaybe )
import Text.ParserCombinators.ReadP ( ReadP , pfail , satisfy , munch1 , count , readP_to_S , (<++) )
import Numeric.Natural ( Natural )
import GHC.Generics ( Generic )
import Data.Binary ( Binary )
--

data Blog = Blog
  { meta :: Page
  , list :: [ Page ]
  } deriving ( Show , Generic , Binary , ToJSON , FromJSON )

data Page = Page
  { name    :: String
  , slug    :: String
  , link    :: String

  , less    :: Maybe String
  , more    :: Maybe String
  , text    :: Maybe String

  , creator :: String
  , pubDate :: Date
  , modDate :: Maybe Date
  } deriving ( Show , Generic , Binary , ToJSON , FromJSON )

data Date = Date
  { dateY :: Natural
  , dateM :: Natural
  , dateD :: Natural
  } deriving ( Show , Generic , Binary , ToJSON )

instance FromJSON Date where
  parseJSON ( String s ) = maybe
    ( fail $ "invalid date Y-M-D, " <> unpack s )
    pure
    ( fmap fst $ listToMaybe $ readP_to_S dateP $ unpack s )
    where
      digiP :: ReadP Char
      digiP = satisfy isDigit

      dashP :: ReadP Char
      dashP = satisfy ( == '-' )

      dateP :: ReadP Date
      dateP = Date <$> dateYP <*  dashP <*> dateMP <*  dashP <*> dateDP

      dateYP :: ReadP Natural
      dateYP = do
        ys <- munch1 isDigit
        maybe
          pfail
          pure
          ( readMaybe ys :: Maybe Natural )

      dateMP :: ReadP Natural
      dateMP = do
        ms <- count 2 digiP <++ ( (:[]) <$> digiP )
        maybe
          pfail
          ( \ m -> case m of
            _ | m > 12 || m == 0 -> pfail
              | otherwise        -> pure m
          )
          ( readMaybe ms )

      dateDP :: ReadP Natural
      dateDP = do
        ds <- count 2 digiP <++ ( (:[]) <$> digiP )
        maybe
          pfail
          ( \ d -> case d of
            _ | d > 31 || d == 0 -> pfail
              | otherwise        -> pure d
          )
          ( readMaybe ds )

  parseJSON invalid      = prependFailure
    "parsing Date failed, "
    ( typeMismatch "Date" invalid )
