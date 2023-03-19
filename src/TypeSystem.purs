module TypeSystem where

import Prelude

import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Structures (BinopCode(..), Ident, Term(..), TermType(..), UnopCode(..))
 
type Env = List (Tuple Ident TermType)

emptyEnv :: Env
emptyEnv = Nil

lookup :: Env -> Ident -> Maybe TermType
lookup Nil _ = Nothing 
lookup ((Tuple id t) : tail) ident = if (id == ident) then Just t else lookup tail ident 

update :: Env -> Ident -> TermType -> Env
update env id t = (Tuple id t) : env

a:: Env
a = (Tuple "tst" Nat) : (Tuple "tst1" Bool) : (Tuple "mult" (Func Nat Nat)) : Nil

opcodeToType :: BinopCode -> (Maybe TermType) -> Maybe TermType
opcodeToType opcode t = case t of 
                        Just Nat -> (case opcode of
                                    Sub  -> Just Nat
                                    Add  -> Just Nat
                                    Mult -> Just Nat
                                    Div  -> Just Nat
                                    Gt   -> Just Bool
                                    Lt   -> Just Bool
                                    Eq   -> Just Bool
                                    Ne   -> Just Bool
                                    _    -> Nothing)
                        Just Bool -> (case opcode of 
                                     And  -> Just Bool
                                     Or   -> Just Bool
                                     _    -> Nothing )
                        _    -> Nothing 

typeInfer :: Env -> Term -> Maybe TermType
typeInfer env expr = case expr of 
    T_false -> Just Bool
    T_true  -> Just Bool
    T_num _ -> Just Nat

    T_var ident -> lookup env ident 
    T_var_system ident -> lookup env ident 

    T_fst (T_pair e1 _) -> typeInfer env e1
    T_fst e1 -> (case typeInfer env e1 of
                    Just (Pair t1 _ ) -> Just t1
                    _        -> Nothing)

    T_snd (T_pair _ e2) -> typeInfer env e2
    T_snd e1 -> (case typeInfer env e1 of
                    Just (Pair _ t2 ) -> Just t2
                    _        -> Nothing)

    (T_pair e1 e2) -> (case (typeInfer env e1) of
                        Just t1 -> (case (typeInfer env e2) of
                                    Just t2 -> Just (Pair t1 t2)
                                    _ -> Nothing)
                        _ -> Nothing 
                        )

    (T_if e1 e2 e3) -> (case typeInfer env e1 of
                    Just Bool -> (case typeInfer env e2 of
                                    Just t2 -> (case typeInfer env e3 of
                                                Just t3 -> (if t2 == t3 then Just t2 else Nothing)
                                                _ -> Nothing)
                                    _ -> Nothing)
                    _ -> Nothing)

    (T_func id t e1) -> (case typeInfer (update env id t) e1 of
                        Just t1 -> Just (Func t t1)
                        _ -> Nothing)

    (T_func_system id t e1) -> (case typeInfer (update env id t) e1 of
                        Just t1 -> Just (Func t t1)
                        _ -> Nothing)

    (T_app e1 e2) -> (case typeInfer env e1 of
                        Just (Func t1 t2) -> if (typeInfer env e2) == Just t1 
                                             then Just t2
                                             else Nothing
                        _ -> Nothing)

    (T_let id t e1 e2) -> if (typeInfer env e1) == Just t 
                          then typeInfer (update env id t) e2
                          else Nothing

    (T_binop opcode e1 e2) ->   let t1 = typeInfer env e1 in
                                let t2 = typeInfer env e2 in
                                if t1 == t2 then
                                (opcodeToType opcode t1)
                                else Nothing           
    
    (T_unop opcode e1) ->   let t1 = typeInfer env e1 in 
                            case opcode of 
                            Not -> if t1 == Just Bool then Just Bool else Nothing  
                            Negate -> if t1 == Just Nat then Just Nat else Nothing  

    (T_natRec e1 e2 e3) ->  if (typeInfer env e1) == Just Nat
                            then (case typeInfer env e3 of
                                    Just t_out -> if (typeInfer env e2) == Just (Func t_out t_out)
                                                then Just t_out
                                                else Nothing 
                                    _ -> Nothing ) 
                            else Nothing 

    -- _ -> Nothing


typeCheck ∷ Term → String
typeCheck expr = case typeInfer emptyEnv expr of 
    Just t -> show t
    Nothing -> "Invalid Type"   



-- Termporario para nao quebrar geracao de lambdaOmega simples
typeInferSimple :: Term -> Maybe TermType
typeInferSimple expr = case expr of 
    T_false -> Just Bool
    T_true  -> Just Bool
    --T_zero  -> Just Nat
    T_num _ -> Just Nat
    -- T_succ e1 -> (case typeInfer e1 of
    --                 Just Nat -> Just Nat
    --                 _        -> Nothing)

    T_fst (T_pair e1 _) -> typeInferSimple e1
    T_fst e1 -> (case typeInferSimple e1 of
                    Just (Pair t1 _ ) -> Just t1
                    _        -> Nothing)

    T_snd (T_pair _ e2) -> typeInferSimple e2
    T_snd e1 -> (case typeInferSimple e1 of
                    Just (Pair _ t2 ) -> Just t2
                    _        -> Nothing)

    (T_pair e1 e2) -> (case (typeInferSimple e1) of
                        Just t1 -> (case (typeInferSimple e2) of
                                    Just t2 -> Just (Pair t1 t2)
                                    _ -> Nothing)
                        _ -> Nothing 
                        )

    (T_if e1 e2 e3) -> (case typeInferSimple e1 of
                    Just Bool -> (case typeInferSimple e2 of
                                    Just t2 -> (case typeInferSimple e3 of
                                                Just t3 -> (if t2 == t3 then Just t2 else Nothing)
                                                _ -> Nothing)
                                    _ -> Nothing)
                    _ -> Nothing)

    _ -> Nothing