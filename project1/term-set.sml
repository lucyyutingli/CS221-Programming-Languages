structure TermSet :> sig

  type set

  val empty : set
  val singleton : NB.term -> set
  val size : set -> int

  val insert : NB.term * set -> set
  val union : set * set -> set
  val toList : set -> NB.term list

end = struct

  type set = unit (* <-- replace this with something else! *)

  val empty = ()  (* <-- replace this with something else! *)
		
  fun singleton _ = raise Fail "todo: TermSet.singleton"

  fun size _ = raise Fail "todo: TermSet.size"

  fun insert _ = raise Fail "todo: TermSet.insert"

  fun union _ = raise Fail "todo: TermSet.union"

  fun toList _ = raise Fail "todo: TermSet.toList"
			      
end
