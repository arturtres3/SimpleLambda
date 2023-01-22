module Term
  ( Term(..)
  , eval
  , showTerm
  , step
  )
  where

import Prelude
import Data.Int(decimal, toStringAs)

import Data.Maybe (Maybe(..))


data Term   = T_true
            | T_false
            | T_zero
            | T_succ Term
            | T_num Int
            | T_if Term Term Term
            | T_pair Term Term
            | T_fst Term
            | T_snd Term

showTerm ∷ Term → String
showTerm t = case t of
            T_true -> "_true"
            T_false -> "_false"
            T_zero  -> "_zero" 
            T_num v -> "(_num " <> (toStringAs decimal v) <> ")"
            T_succ e1 -> "_succ " <> showTerm e1
            T_fst e1 -> "_fst" <> showTerm e1
            T_snd e1 -> "_snd" <> showTerm e1
            T_pair e1 e2 -> "(_pair " <> showTerm e1 <> ", " <> showTerm e2 <> ")"
            T_if t1 t2 t3 -> "(_if " <> 
                            showTerm t1 <> " " <>
                            showTerm t2 <> " " <>
                            showTerm t3 <> ")"


step :: Term -> Maybe Term
step expr = case expr of
    T_false -> Nothing
    T_true  -> Nothing 
    T_zero  -> Nothing 
    T_num _ -> Nothing
    T_succ e1 ->
        (case step e1 of 
            Just e1' -> Just (T_succ e1')
            Nothing  -> Nothing)
    
    (T_pair e1 e2 ) -> 
        (case step e1 of 
            Just e1' -> Just (T_pair e1' e2 )
            Nothing  -> (case step e2 of 
                        Just e2' -> Just (T_pair e1 e2' )
                        Nothing  -> Nothing))

    T_fst (T_pair e1 _ ) -> Just e1
    T_fst e1 -> 
        (case step e1 of 
            Just e1' -> Just (T_fst e1')
            Nothing  -> Nothing)

    T_snd (T_pair _ e2) -> Just e2
    T_snd e1 -> 
        (case step e1 of 
            Just e1' -> Just (T_snd e1')
            Nothing  -> Nothing)
    
    (T_if T_true  e2 _) -> Just e2
    (T_if T_false _ e3) -> Just e3
    (T_if e1 e2 e3) -> 
        (case step e1 of 
            Just e1' -> Just (T_if e1' e2 e3)
            Nothing  -> Nothing)

eval :: Term -> Term
eval expr = case step expr of 
    Just e' -> eval e'
    Nothing -> expr