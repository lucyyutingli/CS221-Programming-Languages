structure Eval : sig

  datatype normal_form
    = Value of InternalAST.term
    | Stuck of InternalAST.term

  val step  : InternalAST.term -> InternalAST.term option
  val steps : InternalAST.term -> InternalAST.term list
  val eval  : InternalAST.term -> normal_form

end = struct

  structure I = InternalAST

  fun unsucc (I.Succ t) = t
    | unsucc _ = raise Fail "unsucc: deep compiler bug"
		  
  fun step I.True = NONE
    | step I.False = NONE
    | step (I.If (I.True, t2, _)) = SOME t2
    | step (I.If (I.False, _, t3)) = SOME t3
    | step (I.If (t1, t2, t3)) =
       (case step t1
 	  of SOME t1' => SOME (I.If (t1', t2, t3))
	   | NONE => NONE)
    | step I.Zero = NONE
    | step (I.Succ t1) =
       (case step t1
	  of SOME t1' => SOME (I.Succ t1')
	   | NONE => NONE)
    | step (I.Pred I.Zero) = SOME I.Zero
    | step (I.Pred t1) =
        if I.isNumericValue t1
        then SOME (unsucc t1)
	else (case step t1
	        of SOME t1' => SOME (I.Pred t1')
		 | NONE => NONE)
    | step (I.Eq (t1, t2)) =
        if I.isNumericValue t1 andalso I.isNumericValue t2
	then (case (t1, t2)
	        of (I.Zero, I.Zero) => SOME I.True
	         | (I.Zero, I.Succ _) => SOME I.False
		 | (I.Succ _, I.Zero) => SOME I.False
		 | (I.Succ nv1, I.Succ nv2) => SOME (I.Eq (nv1, nv2))
		 | _ => raise Fail "compiler bug")
	else if I.isNumericValue t1
	     then (case step t2
 		     of SOME t2' => SOME (I.Eq (t1, t2'))
		      | NONE => NONE)
	     else (case step t1
		     of SOME t1' => SOME (I.Eq (t1', t2))
		      | NONE => NONE)
    | step (I.GT (t1, t2)) =
        if I.isNumericValue t1 andalso I.isNumericValue t2
	then (case (t1, t2)
	        of (I.Zero, _) => SOME I.False
		 | (I.Succ _, I.Zero) => SOME I.True
		 | (I.Succ nv1, I.Succ nv2) => SOME (I.GT (nv1, nv2))
		 | _ => raise Fail "compiler bug")
        else if I.isNumericValue t1
	     then (case step t2
	             of SOME t2' => SOME (I.GT (t1, t2'))
		      | NONE => NONE)
             else (case step t1
	             of SOME t1' => SOME (I.GT (t1', t2))
		      | NONE => NONE)
    | step (I.Plus (t1, t2)) =
        if I.isNumericValue t1 andalso I.isNumericValue t2
	then (case t1
	        of I.Zero => SOME t2
		 | I.Succ nv1 => SOME (I.Plus (nv1, I.Succ t2))
		 | _ => raise Fail "compiler bug")
        else if I.isNumericValue t1
	     then (case step t2
	             of SOME t2' => SOME (I.Plus (t1, t2'))
		      | NONE => NONE)
             else (case step t1
	             of SOME t1' => SOME (I.Plus (t1', t2))
		      | NONE => NONE)
    | step I.Unit = NONE
    | step (I.Pair (t1, t2)) = raise Fail "todo: step, Pair"
    | step (I.Select1 t1) = raise Fail "todo: step, Select1"
    | step (I.Select2 t1) = raise Fail "todo: step, Select2"
    | step (I.Scope (x, t1, t2)) = raise Fail "todo: step, Scope"
    | step (I.Variable _) = NONE
		      
  fun steps t =
    (case step t
       of NONE => [t]
        | SOME t' => t :: steps t')

  datatype normal_form
    = Value of I.term
    | Stuck of I.term

 fun eval t =
   (case step t
      of NONE => if I.isValue t then Value t else Stuck t
       | SOME t' => eval t')

end
