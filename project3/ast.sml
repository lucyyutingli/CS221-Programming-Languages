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
    | Unit
    | Pair of term * term
    | Select1 of term
    | Select2 of term
    | Scope of string * term * term
    | Variable of string
    | Xor of term * term

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
    | Unit
    | Pair of term * term
    | Select1 of term
    | Select2 of term
    | Scope of string * term * term
    | Variable of string
    | Xor of term * term

  infix spc
  fun t1 spc t2 = t1 ^ " " ^ t2

  fun tos True = "#t"
    | tos False = "#f"
    | tos (If (t1, t2, t3)) = "(if" spc (tos t1) spc (tos t2) spc (tos t3) ^ ")"
    | tos (Not t1) = "(not" spc (tos t1) ^ ")"
    | tos (And (t1, t2)) = "(and" spc (tos t1) spc (tos t2) ^ ")"
    | tos (Or (t1, t2)) = "(or" spc (tos t1) spc (tos t2) ^ ")"
    | tos (NatConst n) = Int.toString n
    | tos (Pred t1) = "(pred" spc (tos t1) ^ ")"
    | tos (Isz t1) = "(isz" spc (tos t1) ^ ")"
    | tos (Eq (t1, t2)) = "(=" spc (tos t1) spc (tos t2) ^ ")"
    | tos (GT (t1, t2)) = "(>" spc (tos t1) spc (tos t2) ^ ")"
    | tos (Plus (t1, t2)) = "(+" spc (tos t1) spc (tos t2) ^ ")"
    | tos Unit = "u"
    | tos (Pair (t1, t2)) =  "(pair" spc (tos t1) spc (tos t2) ^ ")"
    | tos (Select1 t1) = "(#1" spc (tos t1) ^ ")"
    | tos (Select2 t1) = "(#2" spc (tos t1) ^ ")"
    | tos (Scope (v, t1, t2)) = ("{" ^ v) spc (tos t1) spc (tos t2) ^ "}"
    | tos (Variable v) = v
    | tos (Xor (t1, t2)) = "(xor" spc (tos t1) spc (tos t2) ^ ")"

end
