module Mastarija.Make where
--
import Slick
import Development.Shake ( Action , ShakeOptions (..) , Verbosity (..) , writeFile' , shakeOptions )
import Development.Shake.Classes ()
import Development.Shake.Forward ( forwardOptions , shakeArgsForward )
import Development.Shake.FilePath ( (</>) )
--
import Data.Text ( unpack )
import Data.Aeson ( toJSON )
--
import Mastarija.Data
--

root :: FilePath
root = "www/"

make :: IO ()
make = do
  let opts = forwardOptions $ shakeOptions
        { shakeVerbosity = Verbose
        , shakeLintInside =
          [ "src/pages/"
          , "src/posts/"
          , "tpl"
          , "tpl/css"
          , "tpl/img"
          , "tpl/jsc"
          ]
        }
  shakeArgsForward opts home

home :: Action ()
home = do
  let site = Site
        { name = "Maštarija"
        , item = Page
          { title   = "Home"
          , short   = "Hello from the abbis"
          , intro   = "I am an introductory text."
          , content = "I am the mainest content of them all, and there are no better contents than me!!!!!"
          }
        }

  homeT <- compileTemplate' "tpl/page.mustache"
  homeH <- pure $ unpack $ substitute homeT $ toJSON site

  writeFile' ( root </> "index.html" ) homeH

blog :: Site [ Page ] -> Action ()
blog = undefined
