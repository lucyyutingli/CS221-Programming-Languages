structure Token : sig

  datatype token
    = LParen
    | RParen 
    | HashT
    | HashF
    | If
    | Not
    | And
    | Or
    | Pred
    | Isz
    | Equals
    | GT
    | Plus
    | NatConst of int
		   
  val tos : token -> string
	    
end = struct

  datatype token
    = LParen
    | RParen 
    | HashT
    | HashF
    | If
    | Not
    | And
    | Or
    | Pred
    | Isz
    | Equals
    | GT
    | Plus
    | NatConst of int
		   
  fun tos LParen = "LParen"
    | tos RParen = "RParen"
    | tos HashT  = "HashT"
    | tos HashF  = "HashF"
    | tos If = "If"
    | tos Not = "Not"
    | tos And = "And"
    | tos Or = "Or"
    | tos Pred = "Pred"
    | tos Isz = "Isz"
    | tos Equals = "Equals"
    | tos GT = "GT"
    | tos Plus = "Plus"
    | tos (NatConst n) = "NatConst( " ^ Int.toString n ^ ")"
						     
end
