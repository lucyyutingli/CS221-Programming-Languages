structure Subst : sig

  val subst : string * InternalAST.term * InternalAST.term -> InternalAST.term

end = struct

  structure I = InternalAST

(* subst (x, v1, t2) means "replace x with v1 in t2" *)
(* ex: subst (".x", I.Zero, I.Variable ".x") --> I.Zero *)
(* ex: subst (".x", I.Zero, I.Variable ".y") --> I.Variable ".y" *)
(* ex: subst (".x", I.Zero, I.True) --> I.True *)
  fun subst (x, v1, I.Variable n) =
        if x = n then v1
        else I.Variable n
    | subst (x, v1, I.True) = I.True
    | subst (x, v1, I.False) = I.False
    | subst (x, v1, I.Zero) = I.Zero
    | subst (x, v1, I.If (x1, y, z)) = I.If(subst(x, v1, x1), subst(x, v1, y), subst(x, v1, z))
    | subst (x, v1, I.Succ n) = I.Succ (subst(x, v1, n))
    | subst (x, v1, I.Pred n) = I.Pred (subst(x, v1, n))
    | subst (x, v1, I.Eq (y, z)) = I.Eq (subst(x,v1,y), subst(x,v1,z))
    | subst (x, v1, I.GT (y, z)) = I.GT (subst(x, v1, y), subst (x, v1, z))
    | subst (x, v1, I.Plus (y, z)) = I.Plus (subst (x, v1, y), subst (x, v1, z))
    | subst (x, v1, I.Unit) = I.Unit
    | subst (x, v1, I.Pair (y, z)) = I.Pair (subst(x, v1, y), subst(x, v1, z))
    | subst (x, v1, I.Select1 t1) = I.Select1 (subst(x, v1, t1))
    | subst (x, v1, I.Select2 t1) = I.Select2 (subst (x, v1, t1))
    | subst (x, v1, I.Scope (name, y, z)) = I.Scope (name, subst (x, v1, y), subst (x, v1, z))


end
