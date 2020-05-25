structure Token = struct

  datatype token
    = Var of string
    | Abbr of string
    | ID of string
    | Nat of int
    | LBrack
    | RBrack
    | LParen
    | RParen
    | Caret
    | LeftArrow
    | Dot
	
  fun tos (Var x) = "Var(" ^ x ^ ")"
    | tos (Abbr a) = "Abbr(" ^ a ^ ")"
    | tos (ID a) = "ID(" ^ a ^ ")"
    | tos (Nat n) = "Nat(" ^ Int.toString n ^ ")"
    | tos LBrack = "LBrack"
    | tos RBrack = "RBrack"
    | tos LParen = "LParen"
    | tos RParen = "RParen"
    | tos Caret = "Caret"
    | tos LeftArrow = "LeftArrow"
    | tos Dot = "Dot"

end
