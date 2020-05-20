structure Desugar : sig

  val desugar : AST.term -> InternalAST.term

end = struct

  structure A = AST
  structure I = InternalAST

  fun succ n =
    if (n<0)
    then raise Fail "negative"
    else if (n=0)
         then I.Zero
         else I.Succ (succ (n-1))

  fun desugar t =
    let
      fun lp A.True = I.True
        | lp A.False = I.False
	| lp (A.If (t1, t2, t3)) = I.If (lp t1, lp t2, lp t3)
	| lp (A.Not t1) = I.If (lp t1, I.False, I.True)
	| lp (A.And (t1, t2)) = I.If (lp t1, lp t2, I.False)
	| lp (A.Or (t1, t2)) = I.If (lp t1, I.True, lp t2)
	| lp (A.NatConst n) = succ n
	| lp (A.Pred t1) = I.Pred (lp t1)
	| lp (A.Isz t1) = I.Eq (lp t1, I.Zero)
	| lp (A.Eq (t1, t2)) = I.Eq (lp t1, lp t2)
	| lp (A.GT (t1, t2)) = I.GT (lp t1, lp t2)
	| lp (A.Plus (t1, t2)) = I.Plus (lp t1, lp t2)
	| lp A.Unit = I.Unit
	| lp (A.Pair (t1, t2)) = I.Pair (lp t1, lp t2)
	| lp (A.Select1 t1) = I.Select1 (lp t1)
	| lp (A.Select2 t1) = I.Select2 (lp t1)
	| lp (A.Scope (x, t1, t2)) = I.Scope (x, lp t1, lp t2)
	| lp (A.Variable v) = I.Variable v
	| lp (A.Xor (t1, t2)) = I.If (I.And (lp t1, lp t2), I.False,
      I.If(I.Or (lp t1, lp t2), I.True, I.False))
    in
      lp t
    end

end
