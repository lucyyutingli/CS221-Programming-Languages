structure Scan : sig


  val nextToken : char list -> (Token.token * char list) option
  val scan : string -> Token.token list

end = struct

  structure T = Token
  val isDigit = Char.isDigit

  fun numToken (n :: cs, list) =
        if (isDigit n)
          then
            numToken (cs, list @ [n])
          else
            (String.implode (list), n::cs)
    | numToken ([], list) = (String.implode list, [])



  fun nextToken [] = NONE
    | nextToken (#"(" :: cs) = SOME (T.LParen, cs)
    | nextToken (#")" :: cs) = SOME (T.RParen, cs)
    | nextToken (#"#" :: #"t" :: cs) = SOME (T.HashT, cs)
    | nextToken (#"#" :: #"f" :: cs) = SOME (T.HashF, cs)
    | nextToken (#"i" :: #"f" :: cs) = SOME (T.If, cs)
    | nextToken (#"n" :: #"o" :: #"t" :: cs) = SOME (T.Not, cs)
    | nextToken (#"a" :: #"n" :: #"d" :: cs) = SOME (T.And, cs)
    | nextToken (#"o" :: #"r" :: cs) = SOME (T.Or, cs)
    | nextToken (#"p" :: #"r" :: #"e" :: #"d" :: cs) = SOME (T.Pred, cs)
    | nextToken (#"i" :: #"s" :: #"z" :: cs) = SOME (T.Isz, cs)
    | nextToken (#"=" :: cs) = SOME (T.Equals, cs)
    | nextToken (#">" :: cs) = SOME (T.GT, cs)
    | nextToken (#"+" :: cs) = SOME (T.Plus, cs)
    | nextToken (#" " :: cs) = nextToken cs
    | nextToken (n :: cs)  =
        if (isDigit (n)) = true
          then let
            val cs' = #2 (numToken ((n :: cs, [])))
            val number = Int.fromString (#1 (numToken ((n :: cs, []))))
          in
            (case number
              of SOME number' => SOME (T.NatConst number', cs')
              | NONE => nextToken cs)
          end
          else
            nextToken cs


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
