module Main where

import Prelude

import CompileOmega (makeLOmega, makeLOmegaDefs, makeLOmegaDefsUsed)
import CompileSTLC (makeSTLC, makeSTLCSimple)
import Effect (Effect)
import Effect.Console (log)
import Structures (Term)
import TokenParser (getTerm)

compileOmega ∷ String → String
compileOmega str = makeLOmega $ getTerm str

compileSTLC :: String -> String 
compileSTLC str = makeSTLC $ getTerm str

compileSTLCSimple :: String -> String 
compileSTLCSimple str = makeSTLCSimple $ getTerm str

compileOmegaDefs :: String -> String 
compileOmegaDefs str = makeLOmegaDefs $ getTerm str

compileOmegaDefsUsed :: String -> String 
compileOmegaDefsUsed str = makeLOmegaDefsUsed $ getTerm str

b ∷ Term
b = getTerm "if true then (if true then true else true) else true" 

main :: Effect Unit
main = do
  --logShow a
  log $ show b
  log $ makeLOmega b
  log $ compileOmega "true"

