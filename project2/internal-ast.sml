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

  fun isNumericValue _ = raise Fail "todo: InternalAST.isNumericValue"
		       
  fun isValue _ = raise Fail "todo: InternalAST.isValue"
		       
  fun tos _ = raise Fail "todo: InternalAST.tos"
		       
end
