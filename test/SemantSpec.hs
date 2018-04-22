module SemantSpec where

import           Protolude
import           System.Directory
import           System.FilePath.Posix
import           Test.Hspec

import           AbSyn
import           Env
import           Semant
import           Symbol
import           Types

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

shouldEvalTo a b = (evalTrans a) `shouldBe` b

shouldError a = isLeft (evalTrans a) `shouldBe` True

shouldErrorWith a e = (evalTrans a) `shouldBe` (Left e)

spec :: Spec
spec = do
  describe "Translate declarations" $ do
    it "inserts var type on declaration" $ do
      let e = emptyStringTable
          (var, t) = symbol "var" e
          (ty, t') = symbol "ty" t
          res = Envs (enter var (VarEntry Types.Int) baseVEnv)baseTEnv
      transDec (VarDec var Nothing (IntExp 10)) `shouldEvalTo` (Right res)
    it "errors if type constraint doesn't match var expression" $ do
      let e = emptyStringTable
          (var, t) = symbol "var" e
          (ty, t') = symbol "ty" t
      transDec (VarDec var (Just ty) (IntExp 10)) `shouldErrorWith` (MismatchedType "")
