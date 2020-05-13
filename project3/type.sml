structure Type = struct

(* no signature on this module becuase it's so simple *)

  datatype ty (* note: the name "type" is not available here, since that word is taken by SML itself *)
    = Nat
    | Bool
    | Unit
    | Product of ty * ty

  fun tos Nat = "Nat"
    | tos Bool = "Bool"
    | tos Unit = "Unit"
    | tos (Product (tau1, tau2)) = "(" ^ tos tau1 ^ " * " ^ tos tau2 ^ ")"
			  
end
