structure Subst : sig

  val subst : string * InternalAST.term * InternalAST.term -> InternalAST.term

end = struct

  structure I = InternalAST

(* subst (x, v1, t2) means "replace x with v1 in t2" *)
(* ex: subst (".x", I.Zero, ".x") --> I.Zero *)
(* ex: subst (".x", I.Zero, I.Variable ".y") --> I.Variable ".y" *)
(* ex: subst (".x", I.Zero, I.True) --> I.True *)
  fun subst (x, v1, t2) = raise Fail "todo: Subst.subst"

end
