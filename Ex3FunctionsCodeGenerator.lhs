> module Ex3FunctionsCodeGenerator where
> import Ex3FunctionsTypes

-----------------------------------------------------------
Solution for Compilers exercise 3

Paul Kelly  Imperial College London  2009
-----------------------------------------------------------

Fill in the gaps...

Part (1): translate function declaration

> transFunction (Defun fname paramname body)
>  = [Define fname] ++ (transExp body allRegs)

Part (2): saving registers

> saveRegs regsNotInUse
>  = []


Part (3): translate expression (ie function body, perhaps including
function calls)

> transExp :: Exp -> [Register] -> [Instr]
> 
> transExp (Const x) (dst:rest) = [Mov (ImmNum x)(Reg dst)]
> transExp (Var x) (dst:rest) = [Mov (ImmName x) (Reg dst)]

Plus Exp Exp
Minux Exp Exp
Apply String Exp

> weight (Const x) = 1

> restoreRegs regsNotInUse
>  = []

