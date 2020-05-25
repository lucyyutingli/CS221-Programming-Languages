structure SULC = struct

  datatype term
    = Var of string
    | Abs of string * term
    | App of term * term
    | Nat of int
    | ID of string
    | Pair of term * term
    | Select1 of term
    | Select2 of term

  datatype program
    = Abbr of string * term * program
    | Term of term

  fun tm (Var x) = x
    | tm (Abs (x, t1)) = "(^" ^ x ^ "." ^ tm t1 ^ ")"
    | tm (App (t1, t2)) = "(" ^ tm t1 ^ " " ^ tm t2 ^ ")"
    | tm (Nat n) = Int.toString n
    | tm (ID a) = a
    | tm (Pair (t1, t2)) = "[" ^ tm t1 ^ " " ^ tm t2 ^ "]"
    | tm (Select1 t1) = tm t1 ^ ".1"
    | tm (Select2 t2) = tm t2 ^ ".2"
				  
  fun prog (Abbr (a, t, p)) = a ^ " <- " ^ tm t ^ "\n" ^ prog p
    | prog (Term t) = tm t
				  
end
