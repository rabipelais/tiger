module ExpToDOT ( expToDOT ) where

import           Protolude hiding (Symbol)

import           AbSyn
import           DOT       (Tree (..), toDOT)
import           Symbol    (Symbol, name)

symbolToTree :: Symbol -> Tree Text
symbolToTree s = Tree (Symbol.name s) []

varToTree :: Var -> Tree Text
varToTree v = case v of
  SimpleVar s      -> symbolToTree s
  FieldVar w s     -> Tree "." [varToTree w, symbolToTree s]
  SubscriptVar w e -> Tree "[]" [varToTree w, expToTree e]

expToTree :: Exp -> Tree Text
expToTree e = case e of
  VarExp v -> varToTree v
  NilExp -> Tree "nil" []
  IntExp i -> Tree (show i) []
  StringExp s -> Tree (stringToLabel s) []
  CallExp f as -> Tree "$Call" ([symbolToTree f] ++ map expToTree as)
  OpExp l o r -> Tree (operToLabel o) [expToTree l, expToTree r]
  RecordExp fs t -> Tree "$Record" ([symbolToTree t] ++ map recordFieldToTree fs)
  SeqExp es -> Tree "$Seq" (map expToTree es)
  AssignExp v x -> Tree ":=" [varToTree v, expToTree x]
  IfExp c t me ->
    Tree "$If" ([expToTree c, expToTree t] ++ maybeToTrees expToTree me)
  WhileExp c b -> Tree "$While" [expToTree c, expToTree b]
  ForExp v l h b ->
    Tree "$For" [symbolToTree v, expToTree l, expToTree h, expToTree b]
  BreakExp -> Tree "$Break" []
  LetExp ds b -> Tree "$Let" (map decToTree ds ++ [expToTree b])
  ArrayExp t s i -> Tree "$Array" [symbolToTree t, expToTree s, expToTree i]

stringToLabel :: Text -> Text
--stringToLabel = concatMap (\c -> if c == '"' then "\\\"" else [c]) . show
stringToLabel x = x

decToTree :: Dec -> Tree Text
decToTree d = case d of
  FunDec fs -> Tree "$Function" [funDecToTree fs]
  VarDec n mt i -> Tree "$Var" ([symbolToTree n] ++
                                  maybeToTrees symbolToTree mt ++
                                  [expToTree i])
  td@(TypeDec n t) -> Tree "$Type" [tyDecTotree td]

tyToTree :: Ty -> Tree Text
tyToTree t = case t of
  NameTy s    -> Tree "$NameTy" [symbolToTree s]
  RecordTy fs -> Tree "$RecordTy" (map fieldToTree fs)
  ArrayTy s   -> Tree "$ArrayTy" [symbolToTree s]

operToLabel :: Oper -> Text
operToLabel o = case o of
  PlusOp   -> "+"
  MinusOp  -> "-"
  TimesOp  -> "*"
  DivideOp -> "/"
  EqOp     -> "="
  NeqOp    -> "<>"
  LtOp     -> "<"
  LeOp     -> "<="
  GtOp     -> ">"
  GeOp     -> ">="
  OrOp     -> "|"
  AndOp    -> "&"

recordFieldToTree :: (Symbol, Exp) -> Tree Text
recordFieldToTree (s, e) = Tree "$AssignField" [symbolToTree s, expToTree e]

fieldToTree :: Field -> Tree Text
fieldToTree (Field n t) = Tree "$Field" [symbolToTree n, symbolToTree t]

funDecToTree :: Function -> Tree Text
funDecToTree (Function n ps mr b) =
  Tree "$Function" ([symbolToTree n] ++
                  map fieldToTree ps ++
                  maybeToTrees symbolToTree mr ++
                  [expToTree b])

tyDecTotree :: Dec -> Tree Text
tyDecTotree (TypeDec n t) = Tree "$TypeDec" [symbolToTree n, tyToTree t]

maybeToTrees :: (a -> Tree Text) -> Maybe a -> [Tree Text]
maybeToTrees f = maybe [] (\x -> [f x])

expToDOT :: Exp -> Text
expToDOT = toDOT . expToTree
