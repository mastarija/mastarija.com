{-# LANGUAGE OverloadedStrings #-}
--
module Mastarija.Maker where
--
import Prelude hiding ( readFile , writeFile )
--
import System.Exit ( ExitCode (..) , exitWith )
import Path ( parseRelDir )
import Path.IO ( copyDirRecur' )
import Lucid ( renderText )
import Data.Ord ( comparing )
import Data.Bool ( bool )
import Data.List ( sortBy )
import Data.Text ( Text , pack , unpack , strip )
import Data.Text.IO ( readFile , writeFile )
import Data.Text.Lazy ( toStrict )
import Data.Bifunctor ( bimap )
import Data.Aeson ( ToJSON (..) , FromJSON (..) )
import Data.Aeson.Types ( parseEither )
import System.FilePath ( normalise , takeBaseName , (</>) )
import System.Directory ( doesFileExist , doesDirectoryExist , listDirectory , createDirectoryIfMissing , removeDirectoryRecursive )
import Control.Monad ( when , forM , forM_ , filterM , (>=>) )
import Control.Monad.IO.Class ( MonadIO (..) )
import Control.Monad.Trans.Except ( ExceptT (..) , except , throwE , runExceptT )
import Control.Monad.Trans.Maybe ( MaybeT (..) , maybeToExceptT )
import Mastarija.Domain ( Tag (..) , Link (..) , Slug (..) , Path (..) , Date (..) , Meta (..) , Article (..) , ArticlePage (..) , Website (..) , TheError (..) )
import Text.MMark ( render , parse , projectYaml )
import Text.Microstache ( PName (..) , Template , renderMustache , compileMustacheDir )
--

maker :: IO ()
maker = do
  let
    mp = fromJust . parseRelDir

  tplt <- compileMustacheDir "main" "tpl"

  removeDirectoryRecursive "www/"
  createDirectoryIfMissing True "www/"

  copyDirRecur' ( mp "tpl/css/" ) ( mp "www/css/" )
  copyDirRecur' ( mp "tpl/jsc/" ) ( mp "www/jsc/" )
  copyDirRecur' ( mp "tpl/img/" ) ( mp "www/img/" )

  result <- runExceptT $ do
    let
      page = loadArticlePage ( Link "/" ) . Path
      main = \ a -> a { link = Link "/" }
      flat = \ a -> a : concatMap flat ( children a )

    home <- page "src/home"
    blog <- page "src/blog"
    code <- page "src/code"
    work <- page "src/work"

    let
      list = [ main home , blog , code , work ]

    forM_ ( concat $ fmap flat list ) $ makeArticlePage tplt

  case result of
    Left e -> do
      putStrLn $ case e of
        InvalidMD p -> "invalid markdown : " <> unPath p
        InvalidYL p -> "invalid yaml : " <> unPath p
        MissingFile p -> "missing file : " <> unPath p
        MissingDirectory p -> "missing directory : " <> unPath p
      exitWith $ ExitFailure 1
    Right _ -> exitWith ExitSuccess

--

normPath :: FilePath -> FilePath
normPath = fmap ( \ c -> bool c '/' $ c == '\\' ) . normalise

listDirs :: FilePath -> IO [ FilePath ]
listDirs path = do
  real <- doesDirectoryExist path
  if not real
    then pure []
    else listDirectory path
          >>= filterM ( doesDirectoryExist >=> pure . not )
          >>= pure . fmap ( path </> )
          >>= pure . fmap normPath

--

loadArticle :: MonadIO m => Path -> ExceptT TheError m Article
loadArticle path = do
  real <- liftIO $ doesFileExist $ unPath path
  when ( not real ) ( throwE $ MissingFile path )

  file <- liftIO $ readFile $ unPath path
  down <- except $ bimap ( const $ InvalidMD path ) id $ parse ( unPath path ) file
  yaml <- maybeToExceptT ( InvalidYL path ) $ MaybeT $ pure $ projectYaml $ down
  meta <- except $ bimap ( const $ InvalidYL path ) id $ parseEither parseJSON yaml
  text <- pure $ let t = toStrict $ renderText $ render down in bool ( Just t ) Nothing ( t == mempty )

  pure $ Article meta text

loadArticlePage :: MonadIO m => Link -> Path -> ExceptT TheError m ArticlePage
loadArticlePage base path = do
  real <- liftIO $ doesDirectoryExist $ unPath path
  when ( not real ) ( throwE $ MissingDirectory path )
  slug <- pure $ Slug $ pack $ takeBaseName $ unPath path
  item <- loadArticle ( Path $ normPath $ unPath path </> ( unpack ( unSlug slug ) <> ".md" ) )
  dirs <- liftIO $ listDirs $ unPath path </> "kids"
  kids <- forM dirs $ \ d ->
      let
      lnk = Link $ normPath $ ( unLink base ) </> ( unpack $ unSlug slug )
      pth = Path $ normPath d
    in
      loadArticlePage lnk pth
  pure $ ArticlePage
    { link = Link $ unLink base </> unpack ( unSlug slug )
    , slug = slug
    , source = path
    , article = item
    , children = sortBy ( \ a b -> comparing ( pubDate . meta . article ) b a ) kids
    , hasChildren = not $ null kids
    }

makeArticlePage :: MonadIO m => Template -> ArticlePage -> ExceptT TheError m ()
makeArticlePage tplt page = do
  let
    path = normPath $ "www/" <> ( unLink $ link $ page )
    file = normPath $ path </> "index.html"

  liftIO $ createDirectoryIfMissing True path
  liftIO $ writeFile file $ toStrict $ renderMustache tplt $ toJSON page
