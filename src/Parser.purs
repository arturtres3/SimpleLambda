module Parser
  ( getTerm
  , parseTerm
  )
  where

import Prelude
import Term (Term(..))

import Data.Either (Either(..) )
import Data.Maybe (Maybe(..))
import Data.Int (fromString)
import Parsing.Combinators (try, (<|>), many1)

import Parsing (Parser, runParser)
import Parsing.String (char, string)
import Parsing.String.Basic (digit)

import Data.String (Pattern(..), Replacement(..), replaceAll)

import Data.Array (fromFoldable)
import Data.String.CodeUnits (fromCharArray)
import Data.List.NonEmpty (toList)

numToNat ∷ Int → Term
numToNat 0 = T_zero
numToNat x = T_succ (numToNat $ x-1 )

parseTerm :: Parser String Term
parseTerm = 
            try (do 
                _ <- char '('
                e <- parseTerm
                _ <- char ')'
                pure e)
        <|>
            try (do 
                _  <- char '('
                e1 <- parseTerm
                _  <- char ','
                e2 <- parseTerm
                _  <- char ')'
                pure (T_pair e1 e2))
        <|>
            try (do 
                _ <- string "true"
                pure T_true)
        <|>
            try (do 
                _ <- string "false"
                pure T_false)
        <|>
            try (do 
                _ <- string "fst"
                e <- parseTerm
                pure (T_fst e))
        <|>
            try (do 
                _ <- string "snd"
                e <- parseTerm
                pure (T_snd e))
        <|>
            try (do 
                n <- many1 digit
                pure (case (fromString $ fromCharArray $ fromFoldable $ toList n) of
                        --Just i -> numToNat i  -- zero e sucessor
                        Just i -> (T_num i)     -- literal natural
                        Nothing -> T_false)
                )
        <|>
            try (do
                _  <- string "if"
                e1 <- parseTerm
                _  <- string "then"
                e2 <- parseTerm
                _  <- string "else"
                e3 <- parseTerm
                pure (T_if e1 e2 e3)
            )

removeSpace ∷ String → String
removeSpace str = replaceAll (Pattern " ") (Replacement "") str

removeTab ∷ String → String
removeTab str = replaceAll (Pattern "\t") (Replacement "") str

removeLine ∷ String → String
removeLine str = replaceAll (Pattern "\n") (Replacement "") str

removeWhiteSpace ∷ String → String
removeWhiteSpace str = removeLine $ removeTab $ removeSpace str

getTerm ∷ String → Term
getTerm inputText = case (runParser (removeWhiteSpace inputText) parseTerm) of
                      Left _ -> T_false
                      Right x -> x


