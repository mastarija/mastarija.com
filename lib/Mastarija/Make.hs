module Mastarija.Make where
--
import Slick
import Development.Shake ( Action )
--
import Mastarija.Data
--

make :: IO ()
make = putStrLn "made"

blog :: Action ()
blog = undefined