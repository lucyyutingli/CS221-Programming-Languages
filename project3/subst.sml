structure Subst : sig

  val subst : string * InternalAST.term * InternalAST.term -> InternalAST.term

end = struct

  structure I = InternalAST

(* subst (x, v1, t2) means "replace x with v1 in t2" *)
(* ex: subst (".x", I.Zero, I.Variable ".x") --> I.Zero *)
(* ex: subst (".x", I.Zero, I.Variable ".y") --> I.Variable ".y" *)
(* ex: subst (".x", I.Zero, I.True) --> I.True *)
  fun subst (x, v1, t2) =
    (case t2
        of I.Variable n =>
            if x = n then v1
            else I.Variable n
         | I.True => I.True
         | I.False => I.False
         | I.If(x1, y, z) =>
              I.If(subst(x, v1, x1), subst(x, v1, y), subst(x, v1, z))
         | I.Zero => I.Zero
         | I.Succ n => I.Succ subst(x,v1,n)
         | I.Pred n => I.Pred subst(x,v1,n)
         | I.Eq y z => I.Eq subst(x,v1,y) subst(x,v1,z)
         | I.GT y z => I.GT subst(x,v1,y) subst(x,v1,z)
         | I.Plus y z => I.Plus subst(x,v1,y) subst(x,v1,z)
         | I.Unit => I.Unit
         | I.Pair (y, z) => I.Pair (subst(x,v1,y), subst(x,v1,z))
         | I.Select1 y => I.Select1 (subst(x,v1,y))
         | I.Select2 y => I.Select2 (subst(x,v1,y))
         | I.Scope (desc, y, z) => I.Scope (desc, subst(x,v1,y), subst(x,v1,z)))


end
