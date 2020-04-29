structure Eval : sig

  val step  : InternalAST.term -> InternalAST.term option
  val steps : InternalAST.term -> InternalAST.term list

  datatype normal_form
    = Value of InternalAST.term
    | Stuck of InternalAST.term
		 
  val eval  : InternalAST.term -> normal_form

end = struct

  structure I = InternalAST

  fun step _ = raise Fail "todo: Eval.step"

  fun steps _ = raise Fail "todo: Eval.steps"

  datatype normal_form
    = Value of I.term
    | Stuck of I.term

  fun eval _ = raise Fail "todo: Eval.eval"

end
