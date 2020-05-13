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
    | Unit
    | Pair of term * term
    | Select1 of term
    | Select2 of term
    | Scope of string * term * term
    | Variable of string

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
    | Unit
    | Pair of term * term
    | Select1 of term
    | Select2 of term
    | Scope of string * term * term
    | Variable of string
		      
  fun isNumericValue Zero = true
    | isNumericValue (Succ t1) = isNumericValue t1
    | isNumericValue _ = false
		       
  fun isValue True = true
    | isValue False = true
    | isValue Zero = true
    | isValue (Succ t1) = isNumericValue t1
    | isValue Unit = true
    | isValue (Pair (t1, t2)) = isValue t1 andalso isValue t2
    | isValue _ = false
		    
  infix spc
  fun s1 spc s2 = s1 ^ " " ^ s2
		       
  fun tos True = "#t"
    | tos False = "#f"
    | tos (If (t1, t2, t3)) = "(if" spc (tos t1) spc (tos t2) spc (tos t3) ^ ")"
    | tos Zero = "0"
    | tos (Succ t1) = "(succ" spc (tos t1) ^ ")"
    | tos (Pred t1) = "(pred" spc (tos t1) ^ ")"
    | tos (Eq (t1, t2)) = "(=" spc (tos t1) spc (tos t2) ^ ")"
    | tos (GT (t1, t2)) = "(>" spc (tos t1) spc (tos t2) ^ ")"
    | tos (Plus (t1, t2)) = "(+" spc (tos t1) spc (tos t2) ^ ")"
    | tos Unit = "u"
    | tos (Pair (t1, t2)) = "(pair" spc (tos t1) spc (tos t2) ^ ")"
    | tos (Select1 t1) = "(#1" spc (tos t1) ^ ")"
    | tos (Select2 t1) = "(#2" spc (tos t1) ^ ")"
    | tos (Scope (x,t1,t2)) = ("{" ^ x) spc (tos t1) spc (tos t2) ^ "}"
    | tos (Variable x) = x
			     
end
