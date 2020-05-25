structure Eval : sig

  val eval      : (ULC.term -> ULC.term option) -> ULC.term -> ULC.term
  val evalSteps : (ULC.term -> ULC.term option) -> ULC.term -> ULC.term list

end = struct

  fun eval step t =
    let
      fun lp t =
        (case step t of
	   SOME t' => lp t'
	 | NONE => t)
      in
        lp t
      end	

  fun evalSteps step t =
    let
      fun lp t =
        (case step t of
	   SOME t' => t :: lp t'
	 | NONE => [t])
      in
        lp t
      end	

end