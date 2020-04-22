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

  fun isNumericValue _ = raise Fail "todo: NB.isNumericValue"
		  
  fun isValue _ = raise Fail "todo: NB.isValue"
			
  fun tos _ = raise Fail "NB.tos"
					      
end
