structure Typecheck : sig

  val typeof : AST.term -> Type.ty
  val check  : AST.term -> AST.term * Type.ty
	      
end = struct

  structure A = AST
  structure T = Type

  type typeEnv = (string * T.ty) list

(* extend : typeEnv * string * ty -> typeEnv *)
(* note: the more recent typings are towards the front of the list *)
  fun extend (g, x, t) = (x,t)::g

(* lookup : typeEnv * string -> ty *)
  fun lookup _ = raise Fail "todo: lookup"

(* typeof : term -> ty *)
(* raise an exception if a term is not well-typed, e.g., (not 0) *)
  fun typeof t = raise Fail "todo: typeof"

  fun check t = (t, typeof t)
	
end
