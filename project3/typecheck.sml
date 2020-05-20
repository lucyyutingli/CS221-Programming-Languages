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
  fun lookup (nil, name) = raise Fail "empty list or name var name not found"
    | lookup ((varname, typename)::tail, name) =
        if varnmae = name then
          typename
        else
          lookup(tail,name)

(* typeof_helper : typeEnv * term -> ty *)
  fun typeof_helper (g, A.True) = T.Bool
    | typeof_helper (g, A.False) = T.Bool
    | typeof_helper (g, A.If (x, t1, t2) =
      (case typeof_helper (g, x)
          of T.Bool =>
            (case typeof_helper (g, t1)
                of sometype =>
                    (case typeof_helper (g, t2)
                        of sometype2 =>
                            if sometype = sometype2 then
                                sometype
                            else
                                raise Fail "If inputs not of the same type"
                        | _ => raise Fail "If inputs not of same type")
                | _ => raise Fail "no type found")
          | _ => raise Fail "If input is not of a bool type")
    | typeof_helper (g, A.Not t1) =
        (case typeof_helper (g, t1)
            of T.Bool => T.Bool
             | _ => raise Fail "type error: not expected a boolean")
    | typeof_helper (g, A.And (t1, t2)) =
        (case typeof_helper (g, t1)
            of T.Bool =>
                (case typeof_helper (g, t2)
                    of T.Bool => T.Bool
                     | _ => raise Fail "type error: and expected a boolean")
             | _ => raise Fail "type error: and expected a boolean")
    | typeof_helper (g, A.Or (t1, t2)) =
        (case typeof_helper (g, t1)
            of T.Bool =>
                (case typeof_helper (g, t2)
                    of T.Bool => T.Bool
                     | _ => raise Fail "type error: and expected a boolean")
             | _ => raise Fail "type error: and expected a boolean")
    | typeof_helper (g, A.NatConst t1) = T.Nat
    | typeof_helper (g, A.Pred t1) =
        (case typeof_helper (g, t1)
            of T.Nat => T.Nat)
             | _ => raise Fail "type error: Pred expected a constant"
    | typeof_helper (g, A.Isz t1) =
        (case typeof_helper (g, t1)
            of T.Nat => T.Nat
             | _ => raise Fail "type error: Isz expected a constant")
    | typeof_helper (g, A.Eq (t1, t2)) =
        (case typeof_helper (g, t1)
            of T.Nat =>
                (case typeof_helper (g,t2)
                    of T.Nat => T.Nat
                     | _ => raise Fail "Eq t2 not of Nat type")
             | _ => raise Fail "Eq t1 not of Nat type")
    | typeof_helper (g, A.GT (t1, t2)) =
        (case typeof_helper (g, t1)
            of T.Nat =>
                (case typeof_helper (g,t2)
                    of T.Nat => T.Nat
                     | _ => raise Fail "GT t2 not of Nat type")
             | _ => raise Fail "GT t1 not of Nat type")
    | typeof_helper (g, A.Plus (t1, t2)) =
          (case typeof_helper (g, t1)
              of T.Nat =>
                  (case typeof_helper (g,t2)
                      of T.Nat => T.Nat
                       | _ => raise Fail "Plus t2 not of Nat type")
               | _ => raise Fail "Plus t1 not of Nat type")
    | typeof_helper (g, A.Unit) = T.Unit
    | typeof_helper (g, A.Pair(t1, t2) = T.Product (typeof_helper (g, t1), typeof_helper (g,t2))
    | typeof_helper (g, A.Select1 t1) =
        (case typeof_helper t1
            of T.Product (typeof_helper (g, t1), typeof_helper (g,t2) => typeof_helper (g,t1))
             | _ => raise Fail "missing element in Select1")
    | typeof_helper (g, A.Select2 t1) =
        (case typeof_helper t1
            of T.Product (typeof_helper (g, t1), typeof_helper (g,t2) => typeof_helper (g,t2))
             | _ => raise Fail "missing element in Select1")
    | typeof_helper (g, A.Scope (s, t1, t2)) =
        extend (g, s, t2)
        (case typeof_helper t2
            of sometype => sometype
             | _ => raise Fail "missing type of t2")
    | typeof_helper (g, A.Variable x) =
        lookup (g, A.Variable x)



(* typeof : term -> ty *)
(* raise an exception if a term is not well-typed, e.g., (not 0) *)
  fun typeof t = typeof_helper ([], t)

  fun check t = (t, typeof t)

end
