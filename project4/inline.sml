structure Inline : sig

  val inline : SULC.program -> SULC.term

end = struct

  fun inline _ = raise Fail "todo: Inline.inline"
					       
end
