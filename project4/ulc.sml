structure ULC = struct

  datatype term
    = Var of string
    | Abs of string * term
    | App of term * term

  fun isValue (Abs _) = true
    | isValue _ = false

  fun tos (Var x) = x
    | tos (Abs (x, t1))  = "(^" ^ x ^ "." ^ tos t1 ^ ")"
    | tos (App (t1, t2)) = "(" ^ tos t1 ^ " " ^ tos t2 ^ ")"
		    
end

