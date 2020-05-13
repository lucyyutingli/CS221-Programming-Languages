structure Token : sig

  datatype token
    = LParen
    | RParen
    | LCurly
    | RCurly
    | HashT
    | HashF
    | If
    | Not
    | And
    | Or
    | Xor
    | Pred
    | Isz
    | Equals
    | GT
    | Plus
    | NatConst of int
    | U
    | Pair
    | Hash1
    | Hash2
    | Variable of string
		   
  val tos : token -> string
	    
end = struct

  datatype token
    = LParen
    | RParen 
    | LCurly
    | RCurly
    | HashT
    | HashF
    | If
    | Not
    | And
    | Or
    | Xor
    | Pred
    | Isz
    | Equals
    | GT
    | Plus
    | NatConst of int
    | U
    | Pair
    | Hash1
    | Hash2
    | Variable of string
	  
  fun tos LParen = "LParen"
    | tos RParen = "RParen"
    | tos LCurly = "LCurly"
    | tos RCurly = "RCurly"
    | tos HashT  = "HashT"
    | tos HashF  = "HashF"
    | tos If = "If"
    | tos Not = "Not"
    | tos And = "And"
    | tos Or = "Or"
    | tos Xor = "Xor"
    | tos Pred = "Pred"
    | tos Isz = "Isz"
    | tos Equals = "Equals"
    | tos GT = "GT"
    | tos Plus = "Plus"
    | tos (NatConst n) = "NatConst( " ^ Int.toString n ^ ")"
    | tos U = "U"
    | tos Pair = "Pair"
    | tos Hash1 = "Hash1"
    | tos Hash2 = "Hash2"
    | tos (Variable v) = "Variable(" ^ v ^ ")"
						     
end
