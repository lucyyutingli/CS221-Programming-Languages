structure Compile : sig

  val evalTyped   : string -> InternalAST.term * Type.ty

  val evalUntyped : string -> Eval.normal_form
  val steps : string -> InternalAST.term list
				  
end = struct

  structure I = InternalAST

  fun evalTyped code =
    let
      val toks = Scan.scan code
      val ast  = Parse.parse toks
      val (ast, tau) = Typecheck.check ast
      val ast' = Desugar.desugar ast
    in
      case Eval.eval ast'
        of Eval.Value v => (v, tau)
	 | Eval.Stuck term => raise Fail "well-typed terms shouldn't get stuck"
    end	

  val evalUntyped = Eval.eval o Desugar.desugar o Parse.parse o Scan.scan
  val steps = Eval.steps o Desugar.desugar o Parse.parse o Scan.scan

end
