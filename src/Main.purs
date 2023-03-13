module Main where

import Prelude

import CompileLambda2 (makeL2, makeL2Defs)
import CompileLambdaC (makeLC, makeLCDefs)
import CompileOmega (makeLOmega, makeLOmegaDefs)
import CompileSTLC (makeSTLC, makeSTLCDefs, makeSTLCSimple)
import Effect (Effect)
import Effect.Console (log)
import Structures (Term)
import TokenParser (getTerm)

compileOmega ∷ String → String
compileOmega str = makeLOmega $ getTerm str

compileSTLC :: String -> String 
compileSTLC str = makeSTLC $ getTerm str

compileSTLCDefs :: String -> String 
compileSTLCDefs str = makeSTLCDefs $ getTerm str

compileSTLCSimple :: String -> String 
compileSTLCSimple str = makeSTLCSimple $ getTerm str

compileOmegaDefs :: String -> String 
compileOmegaDefs str = makeLOmegaDefs $ getTerm str

-- compileOmegaDefsUsed :: String -> String 
-- compileOmegaDefsUsed str = makeLOmegaDefsUsed $ getTerm str

compileL2 :: String -> String 
compileL2 str = makeL2 $ getTerm str

compileL2Defs :: String -> String 
compileL2Defs str = makeL2Defs $ getTerm str

compileLC :: String -> String 
compileLC str = makeLC $ getTerm str

compileLCDefs :: String -> String 
compileLCDefs str = makeLCDefs $ getTerm str

b ∷ Term
b = getTerm "if true then (if true then true else true) else true" 

main :: Effect Unit
main = do
  --logShow a
  log $ show b
  log $ makeLOmega b
  log $ compileOmega "true"

