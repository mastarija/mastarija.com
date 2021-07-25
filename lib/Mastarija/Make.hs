{-# LANGUAGE OverloadedStrings #-}
--
module Mastarija.Make where
--
import Slick ( substitute , compileTemplate' , markdownToHTML )
import Development.Shake ( Action , ShakeOptions (..) , Verbosity (..) , forP , readFile' , writeFile' , shakeOptions , getDirectoryFiles , copyFileChanged )
import Development.Shake.Classes ()
import Development.Shake.Forward ( forwardOptions , shakeArgsForward , cacheAction )
import Development.Shake.FilePath ( takeBaseName , (</>) , (<.>) )
--
import Data.Text ( Text , pack , unpack )
import Data.Aeson ( toJSON , fromJSON , withObject , (.:) , (.:?) )
import Data.Aeson.Types ( Value (..) , Parser (..) , Result (..) , parseEither )
import Control.Monad.IO.Class ( liftIO )
import Text.Mustache.Types ( Template )
import Control.Monad ( void )
--
import Mastarija.Data
--

root :: FilePath
root = "www/"

make :: IO ()
make = do
  opts <- pure shakeOptions
    { shakeLintInside =
      [ "tpl/*"
      , "tpl/css/*"
      , "tpl/jsc/*"
      , "src/pages/*"
      , "src/posts/*"
      ]
    }
  shakeArgsForward opts makeSite

loadTplt :: FilePath -> Action ( Template , FilePath )
loadTplt file = do
  tplt <- compileTemplate' file
  pure ( tplt , file )

makeSite :: Action ()
makeSite = do
  pageT  <- pure "tpl/page.html"
  postT  <- pure "tpl/post.html"
  blogT  <- pure "tpl/blog.html"

  paths  <- getDirectoryFiles "." [ "src/posts/*.md" ]
  mposts <- fmap sequence $ forP paths $ \ p -> makePage postT "blog" p Nothing

  case mposts of
    Nothing     -> fail "failed to build all posts"
    Just posts  -> makeBlog blogT posts

  makeHome pageT "src/pages/home.md"
  makeWork pageT "src/pages/work.md"
  makeCode pageT "src/pages/code.md"

  files <- getDirectoryFiles "./tpl/" [ "img//*" , "css//*" , "jsc//*" ]
  void $ forP files $ \ file ->
    copyFileChanged ( "tpl" </> file ) ( root </> file )

makeMain :: String -> FilePath -> FilePath -> Action ()
makeMain slug tplt path = void $ makePage tplt mempty path $ Just slug

makeHome :: FilePath -> FilePath -> Action ()
makeHome = makeMain "index"

makeBlog :: FilePath -> [ Page ] -> Action ()
makeBlog tpath list' = cacheAction ( "blog" :: Text ) $ do

  paths  <- getDirectoryFiles "." [ "src/posts/*.md" ]
  forP paths $ void . readFile'

  tplt <- compileTemplate' tpath
  epage <- loadPage mempty "src/pages/blog.md" Nothing
  case epage of
    Left error -> do
      fail $ unwords [ "failed to build the Blog:" , error ]
    Right page -> do
      liftIO $ putStrLn $ unwords [ "built the Blog page" ]
      writeFile' ( root </> link page ) . unpack $ substitute tplt $ toJSON $ Blog page list'

makeWork :: FilePath -> FilePath -> Action ()
makeWork = makeMain "work"

makeCode :: FilePath -> FilePath -> Action ()
makeCode = makeMain "code"

loadPage :: FilePath -> FilePath -> Maybe String -> Action ( Either String Page )
loadPage rootPath filePath mslug = do
  file <- readFile' filePath
  json <- markdownToHTML $ pack file

  slug <- pure $ takeBaseName filePath
  link <- pure $ rootPath </> maybe slug id mslug <.> "html"

  pure $ parseEither ( parsePage slug link ) json

makePage :: FilePath -> FilePath -> FilePath -> Maybe String -> Action ( Maybe Page )
makePage tpath rootPath filePath mslug = cacheAction ( "page" :: Text , filePath ) $ do
  tplt  <- compileTemplate' tpath
  epage <- loadPage rootPath filePath mslug

  case epage of
    Left error -> do
      liftIO $ putStrLn $ unlines
        [ "failed to build the Page for [" <> filePath <> "]:"
        , error
        ]
      pure Nothing

    Right page -> do
      writeFile' ( root </> link page ) . unpack $ substitute tplt $ toJSON page
      pure $ Just page

parsePage :: String -> String -> Value -> Parser Page
parsePage slug link = withObject "Page" $ \ v -> Page
  <$> v .: "name"
  <*> ( pure slug )
  <*> ( pure link )
  <*> v .:? "less"
  <*> v .:? "more"
  <*> v .: "content"
  <*> v .: "creator"
  <*> v .: "pubDate"
  <*> v .:? "modDate"
