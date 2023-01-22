module TypeSystem
  ( TermType(..)
  , showType
  , typeCheck
  , typeInfer
  )
  where

import Prelude

import Data.Maybe (Maybe(..))
import Term (Term(..))

data TermType   = Nat
                | Bool
                | Pair TermType TermType

showType ∷ TermType → String
showType t = case t of
            Nat -> "Nat"
            Bool -> "Bool"
            Pair t1 t2 -> "(Pair " <> showType t1 <> " " <> showType t2 <> ")"

typeInfer :: Term -> Maybe TermType
typeInfer expr = case expr of 
    T_false -> Just Bool
    T_true  -> Just Bool
    T_zero  -> Just Nat
    T_num _ -> Just Nat
    T_succ e1 -> (case typeInfer e1 of
                    Just Nat -> Just Nat
                    _        -> Nothing)

    T_fst (T_pair e1 _) -> typeInfer e1
    T_fst e1 -> (case typeInfer e1 of
                    Just (Pair t1 _ ) -> Just t1
                    _        -> Nothing)

    T_snd (T_pair _ e2) -> typeInfer e2
    T_snd e1 -> (case typeInfer e1 of
                    Just (Pair _ t2 ) -> Just t2
                    _        -> Nothing)

    (T_pair e1 e2) -> (case (typeInfer e1) of
                        Just t1 -> (case (typeInfer e2) of
                                    Just t2 -> Just (Pair t1 t2)
                                    _ -> Nothing)
                        _ -> Nothing 
                        )

    (T_if e1 e2 e3) -> (case typeInfer e1 of
                    Just Bool -> (case typeInfer e2 of
                                    Just t2 -> (case typeInfer e3 of
                                                Just t3 -> (if showType t2 == showType t3 then Just t2 else Nothing)
                                                _ -> Nothing)
                                    _ -> Nothing)
                    _ -> Nothing)


typeCheck ∷ Term → String
typeCheck expr = case typeInfer expr of 
    Just t -> showType t
    Nothing -> "Invalid Type"   

