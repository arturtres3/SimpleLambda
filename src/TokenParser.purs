module TokenParser where

import Prelude

import Structures (BinopCode(..), Ident, Term(..), TermType(..), UnopCode(..))
import Tokens (token)

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Identity (Identity)
import Data.List (List(..), (:))
import Data.List.NonEmpty (toList)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested (Tuple3, get1, get2, get3, tuple3)
import Data.Either (Either(..))

import Parsing (Parser, runParser)
import Parsing.Combinators (sepEndBy1, optional, try)
import Parsing.Expr (OperatorTable, Assoc(..), Operator(..), buildExprParser)

type P a = Parser String a

type Param = (Tuple Ident TermType)

parens :: forall a. P a -> P a
parens = token.parens

reservedOp :: String -> P Unit
reservedOp = token.reservedOp

reserved :: String -> P Unit
reserved = token.reserved

identifier :: P Ident
identifier = token.identifier

whiteSpace :: P Unit
whiteSpace = token.whiteSpace

integer :: P Int
integer = token.integer

parseParam :: P Param
parseParam = (do 
               id <- identifier 
               reservedOp ":"
               t <- typeNote
               pure (Tuple id  t))

parseManyParams :: P (List Param)
parseManyParams = (do
                    paramList <- (sepEndBy1 parseParam (reservedOp ";"))
                    pure (toList paramList))
                

makeFunc :: (List Param) -> Term -> Term
makeFunc Nil body = body
makeFunc ((Tuple id t) : tail) body = (T_func id t (makeFunc tail body))

parseFunc :: P Term
parseFunc = (do 
            reserved "func"
            list <- parens parseManyParams
            reservedOp "=>"
            e1 <- expr -- parens (expr)
            pure (makeFunc list e1)
            )

parseLet :: P Term
parseLet = (do
            reserved "let"
            param <- parseParam
            reservedOp "="
            e1 <- expr
            reserved "in"
            e2 <- expr
            pure (T_let (fst param) (snd param) e1 e2)
            )

parseNatRecParams :: (P Term) -> P (Tuple3 Term Term Term) 
parseNatRecParams exp = (do 
                    e1 <- exp
                    reservedOp ";"
                    e2 <- exp
                    reservedOp ";"
                    e3 <- exp
                    pure (tuple3 e1 e2 e3)
                    )


parseNatRec :: P Term
parseNatRec = (do
              reserved "natRec"
              triple <- parens (parseNatRecParams expr)
              pure (T_natRec (get1 triple) (get2 triple) (get3 triple))
              )

parseIf :: P Term 
parseIf = (do 
            reserved "if"
            e1 <- expr
            reserved "then"
            e2 <- expr
            reserved "else"
            e3 <- expr
            pure (T_if e1 e2 e3))

parseFst :: P Term
parseFst = (do 
            reserved "fst"
            (T_fst <$> expr))

parseSnd :: P Term
parseSnd = (do 
            reserved "snd"
            (T_snd <$> expr))

parseTerm :: P Term -> P Term 
parseTerm p = parens p 
            <|> (reserved "true" $> T_true)
            <|> (reserved "false" $> T_false)
            <|> T_var <$> identifier
            <|> T_num <$> integer
            <|> parseFunc
            <|> parseIf
            <|> parseLet
            <|> parseFst
            <|> parseSnd
            <|> parseNatRec

table :: OperatorTable Identity String Term
table =
  [ [ Prefix (reservedOp "!" $> T_unop Not)]
  , [ Infix (reservedOp "*"  $> T_binop Mult) AssocLeft]
  , [ Infix (reservedOp "+"  $> T_binop Add)  AssocLeft
    , Infix (reservedOp "-"  $> T_binop Sub)  AssocLeft]
  , [ Infix (reservedOp "<"  $> T_binop Lt)   AssocLeft
    , Infix (reservedOp ">"  $> T_binop Gt)   AssocLeft 
    , Infix (reservedOp "==" $> T_binop Eq)   AssocLeft 
    , Infix (reservedOp "!=" $> T_binop Ne)   AssocLeft
    , Infix (reservedOp "&&" $> T_binop And)  AssocLeft
    , Infix (reservedOp "||" $> T_binop Or)   AssocLeft]
  , [ Infix (whiteSpace      $> T_app)        AssocLeft]
  , [ Infix (reservedOp ","  $> T_pair)       AssocLeft]
  ]

expr :: P Term
expr = fix allExprs
  where
    allExprs p = buildExprParser table (parseTerm p)


-- faz parsing do espaco em branco antes do primeiro token 
parseExpr :: P Term 
parseExpr = do
          optional (try whiteSpace)
          expr


parseType :: P TermType -> P TermType
parseType p = parens p
        <|> (reserved "Nat" $> Nat)
        <|> (reserved "Bool" $> Bool)

tableType :: OperatorTable Identity String TermType
tableType =
  [ [ Infix (reservedOp "X" $> Pair) AssocLeft
    , Infix (reservedOp "->" $> Func) AssocRight ]]  
    
typeNote :: P TermType
typeNote = fix allTypes
  where
    allTypes p = buildExprParser tableType (parseType p)


getTerm ∷ String → Term
getTerm inputText = case (runParser inputText parseExpr) of
                      Left _ -> T_error
                      Right x -> x

-- runParser "let x:Nat->Bool->Nat = func (a:Nat; b:Bool) = (if true then a else b) in x" expr
--runParser "let fx:Bool->Nat->Nat->Nat = (func (a:Bool; b:Nat; c:Nat) = (if a then b else c)) in (fx true 1 2)" expr

-- getTerm "func (a:Nat) = (a*a)" 
-- antes -->> (binop * (func a Nat (var a) (var a)))
-- depois-->> (func a Nat (binop * (var a) (var a)))
