{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Semant where

import           Protolude hiding (Symbol)

import           AbSyn
import           Env
import           Symbol hiding (name)
import qualified Translate
import           Types

data Envs = Envs {
    vEnv :: VEnv
  , tEnv :: TEnv
  } deriving (Show, Eq)

initialEnvs :: Envs
initialEnvs = Envs baseVEnv baseTEnv

data Error =
  MismatchedType Text
  | UndeclaredType Text
  | IncompatibleTypes Text
  | UnknownIdentifier Text
  deriving (Show)

instance Eq Error where
  MismatchedType _ == MismatchedType _ = True
  UndeclaredType _ == UndeclaredType _ = True
  IncompatibleTypes _ == IncompatibleTypes _ = True
  UnknownIdentifier _ == UnknownIdentifier _ = True
  _ == _ = False

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
inEnv s e = do
  env <- asks e
  case look s env of
    Nothing -> throwError $ UnknownIdentifier (show s)
    Just x  -> return x

actualTy ty = case ty of
  Types.Name _ (Just realTy) -> actualTy realTy
  _                          -> ty

getVarTy entry = case entry of
  FunEntry _ _ -> undefined
  VarEntry ty  -> return $ actualTy ty

transVar :: Var -> Trans ExpTy
transVar var = case var of
  SimpleVar sym -> do
    entry <- sym `inEnv` vEnv
    ty <- getVarTy entry
    returnTy ty

transExp :: AbSyn.Exp -> Trans ExpTy
transExp exp = case exp of
  OpExp left op right -> transOp left op right
  IntExp _            -> returnTy Types.Int
  _                   -> undefined

transOp left op right = do
  expLeft <- transExp left
  expRight <- transExp right
  checkForOper op (typeOf expLeft)
  checkCompatibleTys (typeOf expLeft) (typeOf expRight)
  returnTy Types.Int

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
  throwError $ MismatchedType ("arithmetic type (integer) required but got: " <> (show ty))
  where
    isArithmeticTy Int = True
    isArithmeticTy _   = False

checkComparableTy :: Types.Type -> Trans ()
checkComparableTy ty =
  unless (isComparableTy ty) $
  throwError $ MismatchedType ("type is not comparable: " <> (show ty))
  where
    isComparableTy Int    = True
    isComparableTy String = True
    isComparableTy _      = False

checkEqualityTy :: Types.Type -> Trans ()
checkEqualityTy ty =
  unless (isEqualityTy ty) $
  throwError $ MismatchedType ("type doesn't define equality: " <> (show ty))
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
  throwError $ IncompatibleTypes ("incompatible types " <> (show ty1) <> " and " <> (show ty2))
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

transLet :: [Dec] -> Exp -> Trans ExpTy
transLet decs body = do
  env' <-  transDecs decs
  local (const env') (transExp body)

-- * Type-checking declarations
--
-- `transDec` reads a declaration and returns an updated environment,
-- while `transDecs` modifies the environment after a list of declarations
-- including mutually recursive ones.

-- Translate and modify the environment following the list of declarations.
transDecs :: [Dec] -> Trans Envs
transDecs decs = asks (\env -> foldl transHeaders env decs) >>= \env -> foldM go env decs
  where
    go env d = local (const env) $ transDec d

-- | Passes over the declarations just reading the headers, and
-- puts the symbols of type declarations in the environment
transHeaders :: Envs -> Dec -> Envs
transHeaders (Envs v t) = trHead
  where
    trHead VarDec{} = Envs v t
    trHead (TypeDec name _) = Envs v (enter name (Name name Nothing) t)
    trHead _ = Envs v t

transDec :: Dec -> Trans Envs
transDec dec = uncurry Envs <$> case dec of
  VarDec sym maybeTy val -> transVarDec sym maybeTy val
  TypeDec sym tydec      -> transTypeDec sym tydec
  FunDec (Function name fields maybeRetTy body) -> transFunDec name fields maybeRetTy body

transVarDec sym maybeTy val = do
  valTy <- typeOf <$> transExp val
  tenv <- asks tEnv
  venv <- asks vEnv
  case maybeTy of
    Just ty -> if look ty tenv == Just valTy
              then return (enter sym (VarEntry valTy) venv, tenv)
              else throwError $
                   MismatchedType ("variable " <> (show sym) <> " declared " <> (show ty) <> " but evaled to "<> (show valTy))
    Nothing -> return (enter sym (VarEntry valTy) venv, tenv)

transTypeDec :: (MonadReader Envs m) => Symbol -> Ty -> m (VEnv, TEnv)
transTypeDec sym tydec = do
  tenv <- asks tEnv
  venv <- asks vEnv
  let ty = transTy tenv tydec
  return (venv, enter sym ty tenv)

transFunDec funName fields maybeRetTy body = do
  tenv <- asks tEnv
  venv <- asks vEnv
  let transparam (Field n ty) = do
        case look ty tenv of
          Nothing -> throwError $ UndeclaredType ("Could not translate type " <> (show ty) <> " of arg " <> (show n) <> " on function " <> (show funName))
          Just t -> return t
  params <- mapM transparam fields -- :: m [Type]
      -- Look for the result type. If none given, or not yet defined put a placeholder
  let resultTy = fromMaybe (Name funName Nothing) $ maybeRetTy >>= flip look tenv
      -- Augmented venv with the formal params and return type
      venv' = enter funName (FunEntry params resultTy) venv
      -- Also enter the params in the venv table
      enterparam ven (n, ty) = enter n (VarEntry ty) ven --TODO check access
      typedFields = zip (map fieldName fields) params  -- :: [(Id, Type)]
      venv'' = foldl enterparam venv' typedFields
      newEnv = Envs venv'' tenv
  resultTy' <- typeOf <$> (local (const newEnv) $ transExp body)
  if resultTy' == resultTy || isNothing maybeRetTy -- check if the result type match or if no result type was given
    then return (enter funName (FunEntry params resultTy') venv'', tenv)
    else throwError $
           MismatchedType ("can not unify declared type " <> (show resultTy) <> " with evaled type  "<> (show resultTy') <> " on function " <> (show funName))



transTy :: TEnv -> Ty -> Type
transTy = undefined
