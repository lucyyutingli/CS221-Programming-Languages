structure Scan : sig

  val scan : string -> Token.token list
	    
end = struct

  structure T = Token

(* takeWhile : ('a -> bool) -> ('a list) -> 'a list * 'a list *)
(* take items as long as test passes *)
  fun takeWhile test xs =
    let
      fun cons1 (x, (xs, ys)) = (x::xs, ys)
      fun lp [] = ([], [])
      	| lp (h::t) = if (test h) then cons1 (h, lp t) else ([], h::t)
    in
      lp xs
    end

(* nextInt : char list -> T.token * char list *)
(* pack leading digits into a nat, return that and the rest *)
(* pre: only call if first char is a digit *)
  fun nextInt chars =
   (case takeWhile Char.isDigit chars
      of (digits, cs) =>
        (case Int.fromString (implode digits)
	   of SOME n => (T.NatConst n, cs)
	    | NONE => raise Fail "nextInt bug" (* shouldn't be possible *)))

  fun nextToken chars =
    let
      fun lp [] = NONE
	| lp (#" " :: cs) = lp cs
	| lp (#"\n" :: cs)  = lp cs
	| lp (#"\t" :: cs) = lp cs
	| lp (#"(" :: cs) = SOME (T.LParen, cs)
	| lp (#")" :: cs) = SOME (T.RParen, cs)
	| lp (#"{" :: cs) = SOME (T.LCurly, cs)
	| lp (#"}" :: cs) = SOME (T.RCurly, cs)
	| lp (#"#" :: #"t" :: cs) = SOME (T.HashT, cs)
	| lp (#"#" :: #"f" :: cs) = SOME (T.HashF, cs)
	| lp (#"i" :: #"f" :: cs) = SOME (T.If, cs)
	| lp (#"n" :: #"o" :: #"t" :: cs) = SOME (T.Not, cs)
	| lp (#"a" :: #"n" :: #"d" :: cs) = SOME (T.And, cs)
	| lp (#"x" :: #"o" :: #"r" :: cs) = SOME (T.Xor, cs)
	| lp (#"o" :: #"r" :: cs) = SOME (T.Or, cs)
	| lp (#"p" :: #"r" :: #"e" :: #"d" :: cs) = SOME (T.Pred, cs)
	| lp (#"i" :: #"s" :: #"z" :: cs) = SOME (T.Isz, cs)
	| lp (#"=" :: cs) = SOME (T.Equals, cs)
	| lp (#">" :: cs) = SOME (T.GT, cs)
	| lp (#"+" :: cs) = SOME (T.Plus, cs)
	| lp (#"u" :: cs) = SOME (T.U, cs)
	| lp (#"p" :: #"a" :: #"i" :: #"r" :: cs) = SOME (T.Pair, cs)
	| lp (#"#" :: #"1" :: cs) = SOME (T.Hash1, cs)
	| lp (#"#" :: #"2" :: cs) = SOME (T.Hash2, cs)
	| lp (#"." :: cs) = raise Fail "todo: scan variable names"
	| lp (c::cs) =
	    if Char.isDigit c
	    then SOME (nextInt (c::cs))
	    else raise Fail ("scan error: " ^ implode cs)		
    in
      lp chars
    end

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
