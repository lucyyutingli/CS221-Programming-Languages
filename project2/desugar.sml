structure Desugar : sig

  val desugar : AST.term -> InternalAST.term

end = struct

  structure A = AST
  structure I = InternalAST

  fun desugar (A.Not x) = I.If (desugar x, I.False, I.True)
    | desugar A.True = I.True
    | desugar A.False = I.False
    | desugar (A.Pred x) = I.Pred (desugar x)
    | desugar (A.If (x,y,z)) = I.If (desugar x, desugar y, desugar z)
    | desugar (A.And (x,y)) = I.If (desugar x, desugar y, I.False)
    | desugar (A.Or (x,y)) = I.If (desugar x, I.True, desugar y)
    | desugar (A.Isz x) = I.Eq (desugar x, I.Zero)
    | desugar (A.Eq (x,y)) = I.Eq (desugar x, desugar y)
    | desugar (A.GT (x,y)) = I.GT (desugar x, desugar y)
    | desugar (A.Plus (x,y)) = I.Plus (desugar x, desugar y)
    | desugar (A.NatConst 0) = I.Zero
    | desugar (A.NatConst x) = I.Succ (desugar (A.NatConst (x-1)))

end
