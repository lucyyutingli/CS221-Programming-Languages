structure Parse : sig

  val nextExp : Token.token list -> (AST.term * Token.token list) option
  val parse : Token.token list -> AST.term

end = struct

  structure T = Token
  structure A = AST

  fun nextExp _ = raise Fail "todo: Parse.nextExp"

  fun parse tokens =
    (case nextExp tokens
       of NONE => raise Fail "Parse.parse: no program?"
	| SOME (exp, []) => exp
	| SOME (exp, _) => raise Fail "Parse.parse: too many expressions (should be only one")

end
