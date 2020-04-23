structure Eval : sig

  datatype normal
    = Value of NB.term
    | Stuck of NB.term
	    
  val step  : NB.term -> NB.term option
  val steps : NB.term -> NB.term list
  val eval  : NB.term -> normal

end = struct

  datatype normal
    = Value of NB.term
    | Stuck of NB.term

  fun step _ = raise Fail "todo: Eval.step"

  fun steps _ = raise Fail "todo: Eval.steps"
		 
  fun eval _ = raise Fail "todo: Eval.eval"

end
