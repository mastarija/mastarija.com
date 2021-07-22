{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
--
module Mastarija.Data where
--
import GHC.Generics ( Generic )
import Data.Aeson ( ToJSON , FromJSON )
--

data Site a = Site
  { name :: String
  , item :: a
  } deriving ( Generic , ToJSON , FromJSON )

data Page = Page
  { title         :: String
  , short         :: String
  , intro         :: String
  , content       :: String
  } deriving ( Generic , ToJSON , FromJSON )
