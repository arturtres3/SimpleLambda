# Simple Lambda 

Trabalho de Conclusão de Curso - Ciência da Computação UFRGS 2022/2

SimpleLambda é uma liguagem de programação funcional didática baseada em L1, que pode ser compilada para 4 tipos de Cálculo Lambda Tipado.

Texto: https://lume.ufrgs.br/handle/10183/257991

Compilador online: https://arturtres3.github.io/SimpleLambda/


Sintaxe abstrata de SimpleLambda:

    e ∈ SimpleLambda
    e ::= b | n | e1 op e2 | !e1 | if e1 then e2 else e3
            | x | e1 e2 | func (x : T) ⇒ e | let x : T = e1 in e2
            | (e1, e2) | fst e | snd e
            | natRec (e1; e2; e3)

    T ∈ T ypes
    T ::= Nat | Bool | T1 → T2 | T1 × T2

onde

    n  ∈ {0, 1, 2, 3, 4, ...}               (numeros naturais)
    b  ∈ {true, false}                      (booleanos)
    x  ∈ Ident                              (conjunto de identificadores)
    op ∈ {+, −, ∗, &&, ||, ==, ! =, <, >}   (operaçoes aritméticas e relacionais; e conectivos logicos and e or)