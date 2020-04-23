structure Analysis : sig

  val size : NB.term -> int
  val indexedSet : int -> TermSet.set
			    
end  = struct

  fun size _ = raise Fail "todo: Analysis.size"

  fun indexedSet _ = raise Fail "todo: Analysis.indexedSet"

end
