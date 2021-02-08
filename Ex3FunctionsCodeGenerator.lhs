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

> saveRegs' [] _ = []
> -- If one of the registers is not in the list of used registers, push it
> -- to the stack
> saveRegs' (fst:rest) regsNotInUse
>   | elem fst regsNotInUse = (saveRegs' rest regsNotInUse) 
>   | otherwise = [Mov (Reg fst) Push] ++ (saveRegs' rest regsNotInUse)
> saveRegs regsNotInUse 
> -- Do not push the D0 register as that is the return reg
>  = saveRegs' (delete D0 allRegs) regsNotInUse


Part (3): translate expression (ie function body, perhaps including
function calls)

> -- This function looks ahead and makes sure that there are no needless swaps
> -- in registers -e.g. Mov D0 D1, Mov D1 D0
> peep :: [Instr] -> [Instr]
> peep [] = []
> peep ((Mov (Reg x1) (Reg y1)) : (Mov (Reg x2) (Reg y2)) : rst)
>   | (x1 == y2) && (y1 == x2)  = peep rst
>   | otherwise                 = (Mov (Reg x1) (Reg y1)) : peep ((Mov (Reg x2) (Reg y2)) : rst)
> peep (x:rst) = x : (peep rst)

> transExp :: Exp -> [Register] -> [Instr]
> transExp (Const x) (dst:rest) = [Mov (ImmNum x)(Reg dst)]
> transExp (Var x) (dst:rest) = [Mov (Reg D1) (Reg dst)]
> -- For Plus and Minus, pick the order which will result in the fewest number
> -- of registers used, this is calculated with the weight
> transExp (Plus e1 e2) (dst:nxt:rest) = peep (
>   if (weight e1) > (weight e2)
>   then
>       (transExp e1 (dst:nxt:rest))
>       ++ (transExp e2 (nxt:rest))
>       ++ [Add (Reg nxt) (Reg dst)]
>   else
>       (transExp e2 (nxt:rest))
>       ++ (transExp e1 (dst:rest))
>       ++ [Add (Reg nxt) (Reg dst)] )
> transExp (Minus e1 e2) (dst:nxt:rest) = peep (
>   if (weight e1) > (weight e2)
>   then
>       (transExp e1 (dst:nxt:rest))
>       ++ (transExp e2 (nxt:rest))
>       ++ [Sub (Reg nxt) (Reg dst)]
>   else
>       (transExp e2 (nxt:rest))
>       ++ (transExp e1 (dst:rest))
>       ++ [Sub (Reg nxt) (Reg dst)] )
> -- Save registers and restore them before and after the call is done
> transExp (Apply s e) (dst:rest) = peep (
>   (saveRegs (dst:rest))
>   -- Evaluate the parameter being passed then move it into D1 as this is the 
>   -- parameter register for calling functions
>   ++ (transExp e (delete D1 allRegs))
>   ++ [Mov (Reg D0) (Reg D1)]
>   ++ [Jsr s]
>   ++ (if (dst == D0) then [] else [Mov (Reg D0) (Reg dst)])
>   ++ (restoreRegs (dst:rest)) )

Plus Exp Exp
Minus Exp Exp
Apply String Exp

> -- I have set every action to cost 1 including calling a function
> weight :: Exp -> Int
> weight (Const x) = 1
> weight (Var x) = 1
> weight (Plus e1 e2) = minimum [cost1, cost2]
>   where
>       cost1 = maximum [weight e1, (weight e2) + 1]
>       cost2 = maximum [(weight e1) + 1, weight e2]
> weight (Minus e1 e2) = minimum [cost1, cost2]
>   where
>       cost1 = maximum [weight e1, (weight e2) + 1]
>       cost2 = maximum [(weight e1) + 1, weight e2]
> weight (Apply s e) = (weight e) + 1


> -- This works in the same way as saveRegs but using Pop instead of Push
> restoreRegs' [] _ = []
> restoreRegs' (fst:rest) regsNotInUse
>   | elem fst regsNotInUse = (restoreRegs' rest regsNotInUse)
>   | otherwise = [Mov Pop (Reg fst)] ++ (restoreRegs' rest regsNotInUse)
> restoreRegs regsNotInUse
>  = restoreRegs' (reverse (delete D0 allRegs)) regsNotInUse

