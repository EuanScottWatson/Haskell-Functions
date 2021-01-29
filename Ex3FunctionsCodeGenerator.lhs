> module Ex3FunctionsCodeGenerator where
> import Ex3FunctionsTypes
> import Data.List

-----------------------------------------------------------
Solution for Compilers exercise 3

Paul Kelly  Imperial College London  2009
-----------------------------------------------------------

Fill in the gaps...

Part (1): translate function declaration

> transFunction (Defun fname paramname body)
>  = [Define fname] ++ (transExp body (delete D1 allRegs)) ++ [Ret]

Part (2): saving registers

> saveRegs (fst:rest)
>  = [] 


Part (3): translate expression (ie function body, perhaps including
function calls)

> transExp :: Exp -> [Register] -> [Instr]
> 
> transExp (Const x) (dst:rest) = [Mov (ImmNum x)(Reg dst)]
> transExp (Var x) (dst:rest) = [Mov (Reg D1) (Reg dst)]
> transExp (Plus e1 e2) (dst:nxt:rest) = (transExp e2 (nxt:rest))
>                                     ++ (transExp e1 (dst:nxt:rest))
>                                     ++ [Add (Reg nxt) (Reg dst)]
> transExp (Minus e1 e2) (dst:nxt:rest) = (transExp e2 (nxt:rest))
>                                     ++ (transExp e1 (dst:nxt:rest))
>                                     ++ [Sub (Reg nxt) (Reg dst)]
> transExp (Apply s e) (dst:rest) = (transExp e (dst:rest))
>                                 ++ [Mov (Reg dst) (Reg D1)]
>                                 ++ [Jsr s]

Plus Exp Exp
Minux Exp Exp
Apply String Exp

> weight (Const x) = 1

> restoreRegs regsNotInUse
>  = []

