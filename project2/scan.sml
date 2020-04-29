structure Scan : sig

  val nextToken : char list -> (Token.token * char list) option
  val scan : string -> Token.token list
	    
end = struct

  structure T = Token
		
  fun nextToken _ = raise Fail "todo: Scan.nextToken"

  fun scan code =
    let
      fun lp chars =
        (case nextToken chars
	   of NONE => []
	    | SOME (tok, cs) => tok :: lp cs)
    in
      lp (explode code)
    end

end
