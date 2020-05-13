structure Parse : sig

  val parse : Token.token list -> AST.term

end = struct

  structure T = Token
  structure A = AST

  fun nextExp tokens =
    let
      fun lp [] = NONE
	| lp (T.HashT :: toks) = SOME (A.True, toks)
	| lp (T.HashF :: toks) = SOME (A.False, toks)
	| lp (T.NatConst n :: toks) = SOME (A.NatConst n, toks)
	| lp (T.LParen :: T.Not :: toks) = next1 (A.Not, "not", toks)
	| lp (T.LParen :: T.And :: toks) = next2 (A.And, "and", toks)
	| lp (T.LParen :: T.Or :: toks) = next2 (A.Or, "or", toks)
	| lp (T.LParen :: T.Pred :: toks) = next1 (A.Pred, "pred", toks)
	| lp (T.LParen :: T.Isz :: toks) = next1 (A.Isz, "isz", toks)
	| lp (T.LParen :: T.Equals :: toks) = next2 (A.Eq, "=", toks)
	| lp (T.LParen :: T.GT :: toks) = next2 (A.GT, ">", toks)
	| lp (T.LParen :: T.Plus :: toks) = next2 (A.Plus, "+", toks)
	| lp (T.LParen :: T.If :: toks) = nextIf toks
	| lp (T.U :: toks) = SOME (A.Unit, toks)
	| lp (T.LParen :: T.Pair :: toks) = next2 (A.Pair, "pair", toks)
	| lp (T.LParen :: T.Hash1 :: toks) = next1 (A.Select1, "#1", toks)
	| lp (T.LParen :: T.Hash2 :: toks) = next1 (A.Select2, "#2", toks)
	| lp (T.LCurly :: T.Variable v :: toks) = nextScope (v, toks)
	| lp (T.Variable v :: toks) = SOME (A.Variable v, toks)
	| lp (T.LParen :: T.Xor :: toks) = next2 (A.Xor, "xor", toks)
	| lp (t::_) =  raise Fail ("parse error at " ^ T.tos t)
    in
      lp tokens
    end

  and next1 (constructor, name, toks) =
    (case nextExp toks
       of SOME (e1, T.RParen :: toks1) => SOME (constructor e1, toks1)
	| SOME _ => raise Fail ("parse error: expected RParen after " ^ name)
	| NONE => raise Fail ("parse error: " ^ name ^ " ended unexpectedly"))

  and next2 (constructor, name, toks) =
     (case nextExp toks
        of SOME (e1, toks1) =>
	   (case nextExp toks1
	     of SOME (e2, T.RParen :: toks2) => SOME (constructor (e1, e2), toks2)
	      | SOME _ => raise Fail ("parse error: expected RParen after second arg of" ^ name)
	      | NONE => raise Fail ("parse error: " ^ name ^ " ended unexpectedly after second arg"))
	 | NONE => raise Fail ("parse error: " ^ name ^ " ended unexpectedly after first arg"))

  and nextIf toks = (* no need to generalize to "next3" at the moment b/c if is the only ternary oper *)
      (case nextExp toks
	of SOME (e1, toks1) =>
	    (case nextExp toks1
	       of SOME (e2, toks2) =>
		  (case nextExp toks2
		     of SOME (e3, T.RParen :: toks3) => SOME (A.If (e1, e2, e3), toks3)
		      | SOME _ => raise Fail "parse error: expected RParen after if arg 3" 
		      | NONE => raise Fail "parse error: if ended unexpectedly after arg 3")
		| NONE => raise Fail "parse error: if ended unexpectedly after arg 2")
	 | NONE => raise Fail "parse error: if ended unexpectedly after arg 1")

  and nextScope (v, toks) =
      (case nextExp toks
        of SOME (e1, toks1) =>
	   (case nextExp toks1
	     of SOME (e2, T.RCurly :: toks2) => SOME (A.Scope (v, e1, e2), toks2)
	      | SOME _ => raise Fail ("parse error: expected RCurly after second subterm of scope")
	      | NONE => raise Fail ("parse error: scope ended unexpectedly after second subterm"))
	 | NONE => raise Fail ("parse error: scope ended unexpectedly after first subterm"))
	       
  fun parse tokens =
    (case nextExp tokens
       of NONE => raise Fail "no program at all?"
	| SOME (exp, _::_) => raise Fail "extra expression(s) after the first one"
	| SOME (exp, []) => exp)

end
