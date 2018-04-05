{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Semant where

import           Protolude

import           AbSyn
import           Env
import           Translate
import           Types

data Envs = Envs {
    vEnv :: VEnv
  , tEnv :: TEnv
  } deriving (Show)

initialEnvs :: Envs
initialEnvs = Envs baseVEnv baseTEnv

type Error = Text
type Trans' = StateT [Types.Unique] (ReaderT Envs (ExceptT Error Identity))

newtype Trans a = Trans {unTrans :: Trans' a}
  deriving (Functor, Applicative, Monad
           , MonadReader Envs, MonadState [Types.Unique]
           , MonadError Error)

evalTrans :: Trans a -> Either Error a
evalTrans = runIdentity . runExceptT . flip runReaderT initialEnvs . flip evalStateT Types.uniqueSupply . unTrans

nextUnique :: Trans Types.Unique
nextUnique = do
  (unique:rest) <- get
  put rest
  return unique

--------------------------------------------------------------------------------

data ExpTy = ExpTy {
  translatedExp :: Translate.Exp,
  typeOf        :: Types.Type
  } deriving (Show)

returnTy :: Types.Type -> Trans ExpTy
returnTy = return . ExpTy Translate.Exp

--------------------------------------------------------------------------------

transVar :: Var -> Trans ExpTy
transVar = undefined

transExp :: AbSyn.Exp -> Trans ExpTy
transExp exp = case exp of
  OpExp left op right -> do
    expLeft <- transExp left
    expRight <- transExp right
    checkForOper op (typeOf expLeft)
    checkCompatibleTys (typeOf expLeft) (typeOf expRight)
    returnTy Types.Int
  IntExp _ -> returnTy Types.Int
  _ -> undefined

checkForOper op = case op of
  PlusOp   -> checkArithmeticTy
  MinusOp  -> checkArithmeticTy
  TimesOp  -> checkArithmeticTy
  DivideOp -> checkArithmeticTy
  EqOp     -> checkEqualityTy
  NeqOp    -> checkEqualityTy
  LtOp     -> checkComparableTy
  LeOp     -> checkComparableTy
  GtOp     -> checkComparableTy
  GeOp     -> checkComparableTy

checkArithmeticTy :: Types.Type -> Trans ()
checkArithmeticTy ty =
  unless (isArithmeticTy ty) $
  throwError "arithmetic type (integer) required"
  where
    isArithmeticTy Int = True
    isArithmeticTy _   = False

checkComparableTy :: Types.Type -> Trans ()
checkComparableTy ty =
  unless (isComparableTy ty) $
  throwError "arithmetic type (integer) required" -- TODO
  where
    isComparableTy Int    = True
    isComparableTy String = True
    isComparableTy _      = False

checkEqualityTy :: Types.Type -> Trans ()
checkEqualityTy ty =
  unless (isEqualityTy ty) $
  throwError "arithmetic type (integer) required" -- TODO
  where
    isEqualityTy (Record _ _ ) = True
    isEqualityTy Nil           = True
    isEqualityTy Int           = True
    isEqualityTy String        = True
    isEqualityTy (Array _ _)   = True
    isEqualityTy _             = False


checkCompatibleTys :: Types.Type -> Types.Type -> Trans ()
checkCompatibleTys ty1 ty2 =
  unless (ty1 `isCompatibleWith` ty2) $
  throwError "incompatible types"
  where
    isCompatibleWith (Record _ u1) (Record _ u2) = u1 == u2
    isCompatibleWith Nil (Record _ u2)           = True
    isCompatibleWith (Record _ u1) Nil           = True
    isCompatibleWith Int Int                     = True
    isCompatibleWith String String               = True
    isCompatibleWith (Array _ u1) (Array _ u2)   = u1 == u2
    isCompatibleWith
      (Name _ (Just t1)) (Name _ (Just t2))   = isCompatibleWith t1 t2
    isCompatibleWith Unit Unit               = True



transDec :: Dec -> Trans ExpTy
transDec = undefined

transTy :: Ty -> Trans ExpTy
transTy = undefined
