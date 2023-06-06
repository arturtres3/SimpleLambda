module Main where

import Prelude

import CompileLambda2 (makeL2, makeL2NewSim, makeL2Defs, makeL2DefsNewSim)
import CompileLambda  (makeLambda, makeLambdaDefs)
import CompileLambdaC (makeLC, makeLCDefs)
import CompileOmega (makeLOmega, makeLOmegaNewSim, makeLOmegaDefs, makeLOmegaDefsNewSim)
import CompileSTLC (makeSTLC, makeSTLCNewSim, makeSTLCDefs, makeSTLCDefsNewSim, makeSTLCSimple, makeSTLCSimpleNewSim)
import Effect (Effect)
import Effect.Console (log)
import Structures (Term)
import TokenParser (getTerm)

compileLambda :: String -> String 
compileLambda str = makeLambda $ getTerm str

compileLambdaDefs :: String -> String 
compileLambdaDefs str = makeLambdaDefs $ getTerm str


compileSTLC :: String -> String 
compileSTLC str = makeSTLC $ getTerm str

compileSTLCNewSim :: String -> String 
compileSTLCNewSim str = makeSTLCNewSim $ getTerm str

compileSTLCDefs :: String -> String 
compileSTLCDefs str = makeSTLCDefs $ getTerm str

compileSTLCDefsNewSim :: String -> String 
compileSTLCDefsNewSim str = makeSTLCDefsNewSim $ getTerm str

compileSTLCSimple :: String -> String 
compileSTLCSimple str = makeSTLCSimple $ getTerm str

compileSTLCSimpleNewSim :: String -> String 
compileSTLCSimpleNewSim str = makeSTLCSimpleNewSim $ getTerm str


compileL2 :: String -> String 
compileL2 str = makeL2 $ getTerm str

compileL2NewSim :: String -> String 
compileL2NewSim str = makeL2NewSim $ getTerm str

compileL2Defs :: String -> String 
compileL2Defs str = makeL2Defs $ getTerm str

compileL2DefsNewSim :: String -> String 
compileL2DefsNewSim str = makeL2DefsNewSim $ getTerm str


compileOmega ∷ String → String
compileOmega str = makeLOmega $ getTerm str

compileOmegaDefs :: String -> String 
compileOmegaDefs str = makeLOmegaDefs $ getTerm str

compileOmegaNewSim ∷ String → String
compileOmegaNewSim str = makeLOmegaNewSim $ getTerm str

compileOmegaDefsNewSim :: String -> String 
compileOmegaDefsNewSim str = makeLOmegaDefsNewSim $ getTerm str


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

