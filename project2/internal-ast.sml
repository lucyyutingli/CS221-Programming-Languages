structure InternalAST : sig

  datatype term
    = True
    | False
    | If of term * term * term
    | Zero
    | Succ of term
    | Pred of term
    | Eq of term * term
    | GT of term * term
    | Plus of term * term

  val isValue : term -> bool
  val isNumericValue : term -> bool
  val tos : term -> string

end = struct

  datatype term
    = True
    | False
    | If of term * term * term
    | Zero
    | Succ of term
    | Pred of term
    | Eq of term * term
    | GT of term * term
    | Plus of term * term

  fun isNumericValue Zero = true
    | isNumericValue (Succ x) = isNumericValue(x)
    | isNumericValue _ = false

  fun isValue True = true
    | isValue False = true
    | isValue x = isNumericValue x

  fun tos True = "#t"
    | tos False = "#f"
    | tos Zero = "0"
    | tos (If (x,y,z)) = "(if " ^ tos x ^" "^ tos y ^" "^ tos z ^ ")"
    | tos (Pred x) = "(pred "^tos x^")"
    | tos (Succ x) = "(succ "^tos x^")"
    | tos (Eq (x, y)) = "(= "^tos x^" "^tos y^")"
    | tos (GT (x,y)) = "(> "^tos x^" "^tos y^")"
    | tos (Plus (x,y)) = "(+ "^tos x^" "^tos y^")"

end
