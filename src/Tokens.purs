module Tokens where

--import Prelude

-- https://thimoteus.github.io/posts/2016-05-16-calculator.html

import Parsing.Token (LanguageDef, GenLanguageDef(..), TokenParser, makeTokenParser, alphaNum)
import Parsing.String.Basic (oneOf, lower)
import Parsing.String (char)
import Control.Alt ((<|>))


languageDef :: LanguageDef
languageDef = LanguageDef
  { commentStart: "{-"
  , commentEnd: "-}"
  , commentLine: "--"
  , nestedComments: false
  , identStart: lower -- variaveis comecam com minuscula 
  , identLetter: alphaNum <|> (char '_')
  , opStart: oneOf ['-', ':', '+', '*', '/', '|', '=', '~', '<', ' ', 'X']
  , opLetter: oneOf ['=', '|']
  , reservedNames: ["true", "false", "if", "then", "else", "fst", "snd", "let", "in", "func", "Nat", "Bool", "natRec"]
  , caseSensitive: true
  , reservedOpNames: ["=", "+", "-", "*", "/", "||", "=", "~", "<", ":", "<-", "X"]
}

token :: TokenParser
token = makeTokenParser languageDef
