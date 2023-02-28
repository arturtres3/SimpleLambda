module Evaluate where

-- import Prelude

import Data.Maybe (Maybe(..))
import Structures (Term(..))


step :: Term -> Maybe Term
step expr = case expr of
    T_false -> Nothing
    T_true  -> Nothing 
    T_num _ -> Nothing
    
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


    _ -> Nothing  -- FUNCOES, LET e APP --- Fazer substituicao???

eval :: Term -> Term
eval expr = case step expr of 
    Just e' -> eval e'
    Nothing -> expr