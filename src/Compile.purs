module Compile
  ( makeLOmega
  , makeTypes
  )
  where

import Prelude

import Data.Maybe (Maybe(..))
import Term (Term(..))
import TypeSystem (TermType(..), typeInfer)

makeTypes :: Maybe TermType -> String
makeTypes t = case t of
                Just Bool -> "(forall C:*,C->C->C)"
                Just Nat  -> "(forall C:*, (C -> C) -> C -> C)"
                -- Just (Pair t1 t2) -> "forall C:*, (" 
                --                       <> makeTypes (Just t1) <> 
                --                       " -> " 
                --                       <> makeTypes (Just t2) <> 
                --                       " -> C) -> C"
                Just (Pair t1 t2) -> "((\\A:*, \\B:*, forall C:*, (A -> B -> C) -> C) " 
                                      <> makeTypes (Just t1) <> " "
                                      <> makeTypes (Just t2) <> 
                                      ")"
                Nothing -> "ERRO DE TIPO"

testTypes :: Maybe TermType -> String
testTypes t = case t of
                Just Bool -> "Bool"
                Just Nat  -> "Nat"
                Just (Pair t1 t2) -> "(And " 
                                      <> testTypes (Just t1) <> 
                                      " " 
                                      <> testTypes (Just t2) <> 
                                      ")"
                Nothing -> "ERRO DE TIPO"

        
makeNatural:: Int -> String 
makeNatural 0 = "x"
makeNatural n = "(f " <> (makeNatural (n-1)) <> ")"


termToOmega ∷ Term → String
termToOmega expr = case expr of
            T_true  -> "(\\\\C:*.\\a:C.\\b:C.a)"
            T_false -> "(\\\\C:*.\\a:C.\\b:C.b)"
            T_zero  -> "(\\\\C:*.\\f:C->C.\\x:C.x)"

            T_num n -> "(\\\\C:*.\\f:C->C.\\x:C." <> (makeNatural n) <> ")"

            (T_if e1 e2 e3) ->   "((\\\\D:*.\\c:(forall C:*,C->C->C).\\a:D.\\b:D.(c[D]) a b)" 
                                <> " [" 
                                <> (makeTypes $ typeInfer e2)
                                <> "] " 
                                <> termToOmega e1 <> " "
                                <> termToOmega e2 <> " "
                                <> termToOmega e3 <> ")"

            (T_pair e1 e2) -> "((\\\\A:*. \\\\B:*. \\a: A. \\b: B. \\\\C:*. \\f: A->B->C. f a b)"
                              <> " ["
                              <> (makeTypes $ typeInfer e1)
                              <> "]["
                              <> (makeTypes $ typeInfer e2)
                              <> "] "
                              <> termToOmega e1 <> " "
                              <> termToOmega e2 <> ")"
                  
            T_fst e1 ->  "((\\\\A:*. \\\\B:*. \\p: (\\A:*, \\B:*, forall C:*, (A -> B -> C) -> C) A B. p [A] (\\a: A.\\b: B. a))"
                      <> (case typeInfer e1 of 
                          Just (Pair t1 t2) -> " [" <> (makeTypes $ Just t1) <> "][" <> (makeTypes $ Just t2) <> "] "
                          _ -> "Erro de Tipo [PARES]"
                          )
                      <> termToOmega e1 <> ")"
                    
            T_snd e1 -> "((\\\\A:*. \\\\B:*. \\p: (\\A:*, \\B:*, forall C:*, (A -> B -> C) -> C) A B. p [B] (\\a: A.\\b: B. b))"
                      <> (case typeInfer e1 of 
                          Just (Pair t1 t2) -> " [" <> (makeTypes $ Just t1) <> "][" <> (makeTypes $ Just t2) <> "] "
                          _ -> "Erro de Tipo [PARES]"
                          )
                      <> termToOmega e1 <> ")"
            

            _ -> "incompleto"


makeLOmega :: Term → String
makeLOmega expr = case typeInfer expr of 
                    Just _ -> termToOmega expr
                    Nothing -> "Erro de Tipo"