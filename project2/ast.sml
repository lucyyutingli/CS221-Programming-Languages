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
		       
  fun tos _ = raise Fail "todo: AST.tos"
		       
end
