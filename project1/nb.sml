structure NB : sig

  datatype term
    = True
    | False
    | If of term * term * term
    | Zero
    | Succ of term
    | Pred of term
    | IsZero of term

  val isNumericValue : term -> bool
  val isValue : term -> bool

  val tos : term -> string

end = struct

  datatype term
    = True
    | False
    | If of term * term * term
    | Zero
    | Succ of term
    | Pred of term
    | IsZero of term


  fun isNumericValue Zero = true
    | isNumericValue (Succ x) = isNumericValue(x)
    | isNumericValue _ = false


  fun isValue True = true
    | isValue False = true
    | isValue x = isNumericValue x


  fun tos True = "#t"
  | tos False = "#f"
  | tos Zero = "0"
  | tos (IsZero x) = "(iszero "^tos x^")"
  | tos (Succ x) = "(succ "^tos x^")"
  | tos (Pred x) = "(pred "^tos x^")"
  | tos (If (x, y, z)) = "(if "^tos x^" " ^tos y^ " "^tos z^")"

end
