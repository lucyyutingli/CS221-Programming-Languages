structure AST : sig

  datatype term
    = True
    | False
    | If of term * term * term
    | Not of term
    | And of term * term
    | Or of term * term
    | NatConst of int
    | Pred of term
    | Isz of term
    | Eq of term * term
    | GT of term * term
    | Plus of term * term

  val tos : term -> string

end = struct

  datatype term
    = True
    | False
    | If of term * term * term
    | Not of term
    | And of term * term
    | Or of term * term
    | NatConst of int
    | Pred of term
    | Isz of term
    | Eq of term * term
    | GT of term * term
    | Plus of term * term

  fun tos True = "#t"
    | tos False = "#f"
    | tos (NatConst n) = Int.toString (n)
    | tos (If (x,y,z)) = "(if " ^ tos x ^" "^ tos y ^" "^ tos z ^ ")"
    | tos (Not x) = "(not "^tos x^")"
    | tos (And (x,y)) = "(and "^tos x^ " " ^ tos y^")"
    | tos (Or (x,y)) = "(or "^tos x^" "^tos y^")"
    | tos (Pred x) = "(pred "^tos x^")"
    | tos (Isz x) = "(isz "^tos x^")"
    | tos (Eq (x, y)) = "(= "^tos x^" "^tos y^")"
    | tos (GT (x,y)) = "(> "^tos x^" "^tos y^")"
    | tos (Plus (x,y)) = "(+ "^tos x^" "^tos y^")"

end
