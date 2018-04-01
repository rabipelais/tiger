module Parser where

import           Protolude                  hiding (many, option, try, (<|>))

import qualified Control.Monad.Fail         as Fail

import           Text.Megaparsec
import           Text.Megaparsec.Char       as C
import qualified Text.Megaparsec.Char.Lexer as Tok
import qualified Text.Megaparsec.Expr       as Ex

import           AbSyn
import           Symbol                     hiding (symbol)

type Parser = Parsec Void Text

--(<¿>) = flip (<?>)

sc :: Parser () -- ‘sc’ stands for “space consumer”
sc = Tok.space space1 lineComment blockComment
  where
    lineComment  = Tok.skipLineComment "//"
    blockComment = Tok.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = Tok.lexeme sc

symbol :: Text -> Parser Text
symbol = Tok.symbol sc

integer :: Parser Integer
integer = lexeme Tok.decimal

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

commaSep :: Parser a -> Parser [a]
commaSep p = p `sepBy` (symbol ",")

semiSep :: Parser a -> Parser [a]
semiSep p = p `sepBy` (symbol ";")

validChar = alphaNumChar <|> (char '_')

identifier :: Parser Text
identifier = toS <$> ((lexeme . try) (p >>= check))
  where
    p       = (:) <$> letterChar <*> many validChar
    check x = if x `elem` rws
                then Fail.fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

rws = [ "array", "break", "do", "else", "end", "for", "function", "if", "in"
      , "let", "nil", "of", "then", "to", "type", "var", "while"]

reserved :: Text -> Parser ()
reserved w = (lexeme . try) (C.string w *> notFollowedBy validChar)

charLiteral :: Parser Char
charLiteral = char '\'' *> charLiteral <* char '\''

stringLiteral :: Parser Text
stringLiteral = toS <$> (char '"' >> manyTill Tok.charLiteral (char '"') <* sc)

------------------------------------------------------
-- Parser -------------------------------------------

binary s f assoc = assoc (symbol s >> return (\l r -> OpExp l f r))
prefix s f = Ex.Prefix (symbol s >> return (\x -> OpExp (IntExp 0) f x))

table = [
      [prefix "-" MinusOp],

      [binary "*" TimesOp Ex.InfixL,
      binary "/" DivideOp Ex.InfixL],

      [binary "+" PlusOp Ex.InfixL,
      binary "-" MinusOp Ex.InfixL],

      [binary "=" EqOp Ex.InfixN,
      binary "<>" NeqOp Ex.InfixN,
      binary "<=" LeOp Ex.InfixN,
      binary "<" LtOp Ex.InfixN,
      binary ">=" GeOp Ex.InfixN,
      binary ">" GtOp Ex.InfixN],

      [binary "&" AndOp Ex.InfixR],
      [binary "|" OrOp Ex.InfixR]]

newSymbol i = Symbol i 0
-------------------------------------------------------------------------------
------------------------- Declarations Parsers --------------------------------

-- | Parses the type declarations, following this grammar:
--
-- @
--tydec    -> /type/ type-id = ty
--
--ty       -> type-id
--         -> { tyfields }     (here the braces stand for themselves)
--         -> /array of/ type-id
--
--tyfields -> id : type-id {, id : type-id}
-- @
typeDec :: Parser Dec
typeDec = do
  reserved "type"
  typeid <- identifier
  symbol "="
  (TypeDec (newSymbol typeid)) <$> (try typeArray <|> typeRecord <|>aType)
  where
    typeArray = ArrayTy <$> (reserved "array of" >> identifier >>= return . newSymbol)
    typeRecord = do
      records <- braces $ commaSep record
      return $ RecordTy records
    aType = NameTy . newSymbol <$>  identifier

record :: Parser Field
record = do
  name <- newSymbol <$> identifier
  symbol ":"
  typename <- newSymbol <$> identifier
  return $ Field name typename

-- | Parses a variable declaration:
--
-- @
--vardec -> /var/ id /:=/ exp
--       -> /var/ id : type-id /:=/ exp
-- @
varDec :: Parser Dec
varDec = do
  symbol "var"
  name <- newSymbol <$> identifier
  typeMaybe <- option Nothing (try (do {symbol ":"; x <- newSymbol <$> identifier; return $ Just x}))
  symbol ":="
  exp <- expr
  return $ VarDec name typeMaybe exp

-- | Parses a procedure (that which returns void) or a function
--
-- @
--fundec -> /function/ id ( tyfields ) /=/ exp
--       -> /function/ id ( tyfields ) : type-id /=/ exp
-- @
funDec :: Parser Dec
funDec = FunDec <$> function

function :: Parser Function
function = do
  reserved "function"
  name <- newSymbol <$> identifier
  fields <- parens $ commaSep record
  typeMaybe <- option Nothing (try (do {symbol ":"; x <- newSymbol <$> identifier; return $ Just x}))
  symbol "="
  exp <- try expr <|> parseSeq
  return $ Function name fields typeMaybe exp

dec :: Parser Dec
dec = choice [typeDec, varDec, funDec]

------------------------------------------------------------------------------
-------------------------- Expressions Parsers --------------------------------

-- | Parse @l-values@, which can be normal variables, field accessors,
-- or array subscripts:
--
-- @
--lvalue -> id
--       -> lvalue . id
--       -> lvalue [ expr ]
-- @
-- This grammar is kind of confusing and left recursive, we use the
-- combinator `lfact` as a helper transformation.
var :: Parser Exp
var = VarExp <$> lfact pId (fieldVar <|> subVar)
  where
    fieldVar = do
      symbol "."
      name <- newSymbol <$> identifier
      return (\x -> FieldVar x name)
    subVar = do
      exp <- brackets $ try expr <|> simpleExpr
      return (\x -> SubscriptVar x exp)
    pId = SimpleVar . newSymbol <$> identifier

-- | This combinator does ab* in a left-associative way.
-- Applicable when you have a CFG rule with left recursion
-- which you might rewrite into EBNF X -> YZ*.
lfact :: Parser a -> Parser (a -> a) -> Parser a
lfact p q = do { a <- p
               ; fs <- many q
               ; return (foldl  (\x f -> f x) a fs)
               }

-- | Parse the reserved word __nil__.
nil :: Parser Exp
nil = reserved "nil" >> return NilExp

-- | Parse a sequence of decimal digits as an /int/.
int :: Parser Exp
int = IntExp <$> integer

-- | Parser strings are delimited by quotes (\").
string :: Parser Exp
string = StringExp <$> stringLiteral

binOp = Ex.makeExprParser simpleExpr table

-- | Parses function call of the form @id (args)@
funCall :: Parser Exp
funCall = do
  name <- newSymbol <$> identifier
  args <- parens (commaSep expr)
  return $ CallExp name args

-- | Parses pyte record creation of the form @typeid {id=expr{, id=expr}}@.
recordExp :: Parser Exp
recordExp = do
  typeid <- newSymbol <$> identifier
  records <- braces $ commaSep record
  return $ RecordExp records typeid
  where
    record = do
      name <- newSymbol <$> identifier
      symbol "="
      exp <- expr
      return (name, exp)

-- | A sequence of expressions @{expr{; expr}}@
parseSeq = SeqExp <$> exprs
  where
    exprs = sepBy expr (symbol ";")

-- | An assignment expression @lvar := expr@
assign :: Parser Exp
assign = do
  (VarExp lvar) <- var
  symbol ":="
  exp <- expr
  return $ AssignExp lvar exp

-- | Parses an if statement of the form @if exp1 then exp2 [else exp3]@ where
-- the @else@ part is optional.
parseIf :: Parser Exp
parseIf = do
  reserved "if"
  cond <- expr
  reserved "then"
  res <- expr
  alt <- option Nothing $ Just <$> (reserved "else" >> expr)
  return $ IfExp cond res alt

-- | @/while/ expr1 /do/ expr2@
while :: Parser Exp
while = do
  reserved "while"
  cond <- expr
  reserved "do"
  body <- expr
  return $ WhileExp cond body

-- | @/for/ id := expr1 /to/ expr2 /do/ expr3@.
for :: Parser Exp
for = do
  reserved "for"
  name <- newSymbol <$> identifier
  symbol ":="
  lo <- expr
  reserved "to"
  hi <- expr
  reserved "do"
  body <- expr
  return $ ForExp name lo hi body

-- | @/let/ decs /in/ expseq /end/@.
parseLet :: Parser Exp
parseLet = do
  reserved "let"
  decs <- many dec
  reserved "in"
  exp <- parseSeq
  reserved "end"
  return $ LetExp decs exp

-- | @break@ statement.
break :: Parser Exp
break = reserved "break" >> return BreakExp

-- | An array @typeid [exp1] of exp2@, where @exp1@ is the size and
-- @exp2@ the initial values in the array
array :: Parser Exp
array = do
  typeid <- newSymbol <$> identifier
  size <- brackets expr
  reserved "of"
  initVal <- expr
  return $ ArrayExp typeid size initVal

simpleExpr :: Parser Exp
simpleExpr = choice
  [ nil
  , int
  , Parser.string
  , parens parseSeq
  , parens binOp
  , try funCall
  , try recordExp
  , try array
  , try assign
  , var]

noop = try (symbol "(" >> symbol ")") >> (return $ SeqExp [])

expr :: Parser Exp
expr = choice
  [ noop
  , Parser.break
  , parseIf
  , while
  , Parser.for
  , parseLet
  , binOp
  , parseSeq]

-- | A valid Tiger program is an expression, possibly with whitespace before.
parseProgram :: Parser Exp
parseProgram = between sc (sc <|> eof) expr

parser :: Text -> Text -> Either (ParseError (Token Text) Void) Exp
parser name input = parse parseProgram (toS name) (toS input)
