module ParserSpec where

import           Protolude
import           System.Directory
import           System.FilePath.Posix
import           Test.Hspec
import           Test.Hspec.Megaparsec

import           Parser                (parser)

main :: IO ()
main =
  hspec spec

getFiles f = do
  dir <- map (f </>) <$> getDirectoryContents f
  let files = sort $ filter (\f -> takeExtension f == ".tig") dir
  sources <- mapM readFile files
  return (zip files sources)

getSources :: [Text] -> IO [(Text, Text)]
getSources fs = do
  sources <- mapM (readFile . toS) fs
  return (zip fs sources)

testCases = map (\n -> "test/testcases/test" <> (show n :: Text) <> ".tig") [1..49]

spec :: Spec
spec = do
  tests <- runIO (getSources testCases)
  describe "Parse success tests" $ do
    forM_ tests $ \(f, s) ->
      it ("parses " <> toS f) $ do
        parser (toS f) `shouldSucceedOn` s
