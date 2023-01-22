module Main
  ( b
  , main
  , testFunction
  )
  where

import Prelude

import Parser -- arquivo
import Compile
import Term (showTerm)


import Effect (Effect)
import Effect.Console (log)

testFunction ∷ String → String
testFunction str = makeLOmega $ getTerm str

b = getTerm "if true then (if true then true else true) else true" 

main :: Effect Unit
main = do
  --logShow a
  log $ showTerm b
  log $ makeLOmega b
  log $ testFunction "true"

