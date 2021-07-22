module Mastarija.Data where
--

data Site a = Site
  { name :: String
  , item :: a
  }

data Page a = Page
  { title         :: String
  , subTitle      :: String
  , intro         :: String
  , mainContent   :: String
  }
