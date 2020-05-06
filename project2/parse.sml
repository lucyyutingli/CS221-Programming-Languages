structure Parse : sig

  val nextExp : Token.token list -> (AST.term * Token.token list) option
  val parse : Token.token list -> AST.term

end = struct

  structure T = Token
  structure A = AST

  fun nextExp [] = NONE
    | nextExp (T.HashT :: toks) = SOME (A.True, toks)
    | nextExp (T.HashF :: toks) = SOME (A.False, toks)
    | nextExp (T.NatConst int :: toks) = SOME (A.NatConst int, toks)
    | nextExp (T.LParen :: T.If :: toks) =
        (case (nextExp toks)
            of SOME (t1 , toks') =>
                (case (nextExp toks')
                  of SOME (t2 , toks'') =>
                    (case (nextExp toks'')
                      of SOME (t3, T.RParen :: toks''') =>
                        SOME (A.If (t1, t2, t3), toks''')
                       | NONE => raise Fail "unexpected end of term"
                       | _ => raise Fail "missing term")
                  | NONE => raise Fail "unexpected end of term")
             | NONE => raise Fail "unexpected end of term")

    | nextExp (T.LParen :: T.Not :: toks) =
        (case (nextExp toks)
            of SOME (t1 , T.RParen :: toks') =>
                SOME (A.Not t1, toks')
             | NONE => raise Fail "unexpected end of term"
             | _ => raise Fail "missing term")

    | nextExp (T.LParen :: T.And :: toks) =
        (case (nextExp toks)
            of SOME (t1 , toks') =>
                (case (nextExp toks')
                  of SOME (t2 , T.RParen :: toks'') =>
                      SOME (A.And (t1, t2), toks'')
                       | NONE => raise Fail "unexpected end of term"
                       | _ => raise Fail "missing term")
                  | NONE => raise Fail "unexpected end of term")

    | nextExp (T.LParen :: T.Or :: toks) =
        (case (nextExp toks)
            of SOME (t1 , toks') =>
                (case (nextExp toks')
                  of SOME (t2 , T.RParen :: toks'') =>
                      SOME (A.Or (t1, t2), toks'')
                       | NONE => raise Fail "unexpected end of term"
                       | _ => raise Fail "missing term")
                  | NONE => raise Fail "unexpected end of term")

    | nextExp (T.LParen :: T.Pred :: toks) =
        (case (nextExp toks)
            of SOME (t1 , T.RParen :: toks') => SOME (A.Pred t1, toks')
             | NONE => raise Fail "unexpected end of term"
             | _ => raise Fail "missing term")

    | nextExp (T.LParen :: T.Isz :: toks) =
        (case (nextExp toks)
            of SOME (t1 , T.RParen :: toks') => SOME (A.Isz t1, toks')
             | NONE => raise Fail "unexpected end of term"
             | _ => raise Fail "missing term")

    | nextExp (T.LParen :: T.Equals :: toks) =
        (case (nextExp toks)
            of SOME (t1 , toks') =>
                (case (nextExp toks')
                  of SOME (t2 , T.RParen :: toks'') =>
                      SOME (A.Or (t1, t2), toks'')
                       | NONE => raise Fail "unexpected end of term"
                       | _ => raise Fail "missing term")
                  | NONE => raise Fail "unexpected end of term")

    | nextExp (T.LParen :: T.GT :: toks) =
        (case (nextExp toks)
            of SOME (t1 , toks') =>
                (case (nextExp toks')
                  of SOME (t2 , T.RParen :: toks'') =>
                      SOME (A.Or (t1, t2), toks'')
                       | NONE => raise Fail "unexpected end of term"
                       | _ => raise Fail "missing term")
                  | NONE => raise Fail "unexpected end of term")

    | nextExp (T.LParen :: T.Plus :: toks) =
        (case (nextExp toks)
            of SOME (t1 , toks') =>
                (case (nextExp toks')
                  of SOME (t2 , T.RParen :: toks'') =>
                      SOME (A.Or (t1, t2), toks'')
                       | NONE => raise Fail "unexpected end of term"
                       | _ => raise Fail "missing term")
                  | NONE => raise Fail "unexpected end of term")
    | nextExp (T.LParen :: _ :: toks) = raise Fail "unexpected token after lparen"
    | nextExp _ = raise Fail "unrecognized token"


  fun parse tokens =
    (case nextExp tokens
       of NONE => raise Fail "Parse.parse: no program?"
	| SOME (exp, []) => exp
	| SOME (exp, _) => raise Fail "Parse.parse: too many expressions (should be only one")

end
