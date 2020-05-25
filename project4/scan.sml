structure Scan : sig

  val scan : string -> Token.token list

end = struct

  structure T = Token

  fun collect test items =
    let
      fun cons1 x (xs, ys) = (x::xs, ys)
      fun lp [] = ([], [])
	| lp (all as x::xs) =
	    if test x
	    then cons1 x (lp xs)
	    else ([], all)
    in
      lp items
    end

  fun nat f cs = (* pass first char as f *)
    (case collect Char.isDigit cs of
	 (digits, cs') =>
	   (case Int.fromString (implode (f::digits)) of
		SOME n => SOME (T.Nat n, cs')
	      | NONE => raise Fail "scanner bug (nat)"))

  fun var f cs = (* pass first char as f *)
    (case collect Char.isLower cs of
	 (lowers, cs') => SOME (T.Var (implode (f::lowers)), cs'))

  fun abbr cs = (* no need to pass the initial underscore *)
    (case collect Char.isLower cs of
	 ([], _) => raise Fail "scan error: \"_\" is not a legal abbreviation name"
       | (lowers, cs') => SOME (T.Abbr (implode (#"_"::lowers)), cs'))
	  
  fun id f cs = (* pass first char as f *)
    (case collect Char.isUpper cs of
	 (uppers, cs') => SOME (T.ID (implode (f::uppers)), cs'))
      
  fun nextToken [] = NONE
    | nextToken (#" " :: cs) = nextToken cs
    | nextToken (#"\t":: cs) = nextToken cs
    | nextToken (#"\n":: cs) = nextToken cs
    | nextToken (#"[" :: cs) = SOME (T.LBrack, cs)
    | nextToken (#"]" :: cs) = SOME (T.RBrack, cs)
    | nextToken (#"(" :: cs) = SOME (T.LParen, cs)
    | nextToken (#")" :: cs) = SOME (T.RParen, cs)
    | nextToken (#"^" :: cs) = SOME (T.Caret, cs)
    | nextToken (#"." :: cs) = SOME (T.Dot, cs)
    | nextToken (#"<" :: #"-" :: cs) = SOME (T.LeftArrow, cs)
    | nextToken (c :: cs) =
      if Char.isDigit c      then nat c cs
      else if Char.isLower c then var c cs
      else if c = #"_"       then abbr cs
      else if Char.isUpper c then id c cs
      else raise Fail ("scan error: " ^ implode (c::cs))

  fun scan code =
    let
      fun lp [] = []
	| lp cs = (case nextToken cs of
		       SOME (tok, cs') => tok :: lp cs'
		     | NONE => [])
    in
      lp (explode code)		    
    end

end
