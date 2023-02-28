module Main where

import Prelude

import TokenParser (getTerm)
import Compile (makeLOmega)
import CompileSTLC (makeSTLC, makeSTLCSimple)
import Structures (Term)


import Effect (Effect)
import Effect.Console (log)

testFunction ∷ String → String
testFunction str = makeLOmega $ getTerm str

compileSTLC :: String -> String 
compileSTLC str = makeSTLC $ getTerm str

compileSTLCSimple :: String -> String 
compileSTLCSimple str = makeSTLCSimple $ getTerm str

b ∷ Term
b = getTerm "if true then (if true then true else true) else true" 

main :: Effect Unit
main = do
  --logShow a
  log $ show b
  log $ makeLOmega b
  log $ testFunction "true"

