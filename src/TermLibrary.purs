module TermLibrary where

--import Prelude

import Structures (BinopCode(..), Term(..), TermType(..), UnopCode(..))


shiftIncTerm :: Term
shiftIncTerm = (T_func_system "p" (Pair Nat Nat) 
                        (T_pair (T_snd (T_var_system "p"))  
                        (T_binop  Add  (T_snd (T_var_system "p")) (T_num 1)))
                    ) 

predTerm :: Term 
predTerm  = (T_func_system "n" Nat 
                (T_fst (
                        (T_natRec (T_var_system "n") shiftIncTerm (T_pair (T_num 0) (T_num 0)))
                    )
                )   
            )

subTerm :: Term 
subTerm  = (T_func_system "n" Nat (T_func_system "m" Nat 

                (T_natRec (T_var_system "m") predTerm (T_var_system "n"))

            ))

isZeroTerm :: Term 
isZeroTerm  = (T_func_system "n"  Nat
                (T_natRec (T_var_system "n") (T_func_system "b" Bool (T_false)) (T_true)) 
            )


-- eq = \m n. and (isZero (sub m n)) (isZero (sub n m));
eqTerm :: Term 
eqTerm = (T_func_system "n" Nat (T_func_system "m" Nat   
            (T_binop And 
                (T_app isZeroTerm 
                    (T_app (T_app subTerm (T_var_system "m")) (T_var_system "n") )
                )
                (T_app isZeroTerm 
                    (T_app (T_app subTerm (T_var_system "n")) (T_var_system "m") )
                )    
            )
        ))

neTerm :: Term 
neTerm = (T_func_system "n" Nat (T_func_system "m" Nat 
            (T_unop Not 
                (T_binop And 
                    (T_app isZeroTerm 
                        (T_app (T_app subTerm (T_var_system "m")) (T_var_system "n") )
                    )
                    (T_app isZeroTerm 
                        (T_app (T_app subTerm (T_var_system "n")) (T_var_system "m") )
                    )    
                )
            )
        ))


-- gte = \m n. isZero (sub n m);
gteTerm :: Term 
gteTerm = (T_func_system "n" Nat (T_func_system "m" Nat 
            (T_app isZeroTerm 
                (T_app (T_app subTerm (T_var_system "n")) (T_var_system "m") )
            )
        ))


-- lte = \m n. isZero (sub m n);
lteTerm :: Term 
lteTerm = (T_func_system "n" Nat (T_func_system "m" Nat 
            (T_app isZeroTerm 
                (T_app (T_app subTerm (T_var_system "m")) (T_var_system "n") )
            )
        ))


-- gt = \m n. not (isZero (sub m n));
gtTerm :: Term 
gtTerm = (T_func_system "n" Nat (T_func_system "m" Nat
            (T_unop Not 
                (T_app isZeroTerm 
                    (T_app (T_app subTerm (T_var_system "n")) (T_var_system "m") )
                )
            )
        ))


-- lt = \m n. not (isZero (sub n m));
ltTerm :: Term 
ltTerm = (T_func_system "n" Nat (T_func_system "m" Nat
            (T_unop Not 
                (T_app isZeroTerm 
                    (T_app (T_app subTerm (T_var_system "m")) (T_var_system "n") )
                )
            )
        ))



    