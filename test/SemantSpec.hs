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
    let e = emptyStringTable
    it "inserts var type on declaration" $ do
      let (var, t) = symbol "var" e
          (ty, t') = symbol "ty" t
          res = Envs (enter var (VarEntry Types.Int) baseVEnv) baseTEnv
      transDec (VarDec var Nothing (IntExp 10)) `shouldEvalTo` (Right res)
    it "errors if type constraint doesn't match var expression" $ do
      let (var, t) = symbol "var" e
          (ty, t') = symbol "ty" t
      transDec (VarDec var (Just ty) (IntExp 10)) `shouldErrorWith` (MismatchedType "")
    it "enters simple type into type environment" $ do
      let (var, t) = symbol "tyvar" e
          (ty, t') = symbol "int" t
          res = Envs baseVEnv (enter var Types.Int baseTEnv)
      transDec (TypeDec var (NameTy ty)) `shouldEvalTo` (Right res)
    it "enters simple type from another type" $ do
      let (var, t) = symbol "tyvar" e
          (ty, t') = symbol "int" t
          (var', t'') = symbol "tyvarr" t'
          res = Envs baseVEnv (enter var' Types.Int $ enter var Types.Int baseTEnv)
      transDecs [TypeDec var (NameTy ty), TypeDec var' (NameTy var)] `shouldEvalTo` (Right res)
    it "enters array type into type environment" $ do
      let (var, t) = symbol "tyvar" e
          (ty, t') = symbol "int" t
          res = Envs baseVEnv (enter var (Types.Array Types.Int (Unique 0)) baseTEnv)
      transDec (TypeDec var (ArrayTy ty)) `shouldEvalTo` (Right res)
    it "enters variable of array type into environment" $ do
      let (var, t) = symbol "tyvar" e
          (ty, t') = symbol "int" t
          (var', t'') = symbol "arrayvar" t'
          res = Envs (enter var' (VarEntry (Types.Array Types.Int (Unique 0))) baseVEnv) (enter var (Types.Array Types.Int (Unique 0)) baseTEnv)
      transDecs [TypeDec var (ArrayTy ty), VarDec var' (Just var) (ArrayExp var (IntExp 10) (IntExp 0))] `shouldEvalTo` (Right res)
    it "enters function into environment" $ do
      let (fun, t) = symbol "fun" e
          res = Envs (enter fun (FunEntry [] Types.Int) baseVEnv) baseTEnv
      transDec (FunDec (Function fun [] Nothing (IntExp 10))) `shouldEvalTo` (Right res)
    it "typechecks function body and type" $ do
      let (fun, t) = symbol "fun" e
          res = Envs (enter fun (FunEntry [] Types.Int) baseVEnv) baseTEnv
      transDec (FunDec (Function fun [] (Just (Symbol "int")) (IntExp 10))) `shouldEvalTo` (Right res)
    it "errors if function body and type don't match" $ do
      let (fun, t) = symbol "fun" e
      transDec (FunDec (Function fun [] (Just (Symbol "string")) (IntExp 10))) `shouldErrorWith` (MismatchedType "")

  describe "Tranlate expressions" $ do
    describe "Op arithmetic" $ do
      it "type checks 1 + 2" $ do
        (typeOf <$> (transExp (OpExp (IntExp 1) PlusOp (IntExp 2)))) `shouldEvalTo` (Right Types.Int)
