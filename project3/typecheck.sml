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
  fun lookup (nil, name) = raise Fail "empty list or var name not found"
    | lookup ((varname, typename)::tail, name) =
        if varname = name then
          typename
        else
          lookup(tail, name)

(* typeof_helper : typeEnv * term -> ty *)
  fun typeof_helper (g, A.True) = T.Bool
    | typeof_helper (g, A.False) = T.Bool
    | typeof_helper (g, A.If (x, t1, t2)) =
      (case typeof_helper (g, x)
          of T.Bool =>
            if typeof_helper(g, t1) = typeof_helper(g, t2) then
              typeof_helper(g, t2)
            else
              raise Fail "this should not be possible"
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
                     | _ => raise Fail "type error: or expected a boolean")
             | _ => raise Fail "type error: or expected a boolean")
    | typeof_helper (g, A.Xor (t1, t2)) =
        (case typeof_helper (g, t1)
            of T.Bool =>
                (case typeof_helper (g, t2)
                    of T.Bool => T.Bool
                     | _ => raise Fail "type error: xor expected a boolean")
             | _ => raise Fail "type error: xor expected a boolean")
    | typeof_helper (g, A.NatConst t1) = T.Nat
    | typeof_helper (g, A.Pred t1) =
        (case typeof_helper (g, t1)
            of T.Nat => T.Nat
             | _ => raise Fail "type error: Pred expected a constant")
    | typeof_helper (g, A.Isz t1) =
        (case typeof_helper (g, t1)
            of T.Nat => T.Bool
             | _ => raise Fail "type error: Isz expected a constant")
    | typeof_helper (g, A.Eq (t1, t2)) =
        (case typeof_helper (g, t1)
            of T.Nat =>
                (case typeof_helper (g,t2)
                    of T.Nat => T.Bool
                     | _ => raise Fail "Eq t2 not of Nat type")
             | _ => raise Fail "Eq t1 not of Nat type")
    | typeof_helper (g, A.GT (t1, t2)) =
        (case typeof_helper (g, t1)
            of T.Nat =>
                (case typeof_helper (g,t2)
                    of T.Nat => T.Bool
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
    | typeof_helper (g, A.Pair(t1, t2)) = T.Product (typeof_helper (g, t1), typeof_helper (g,t2))
    | typeof_helper (g, A.Select1 t1) =
        (case t1
            of A.Pair (x, y) =>
              if typeof_helper(g, A.Pair (x, y)) = T.Product (typeof_helper (g, x), typeof_helper (g,y)) then
                typeof_helper (g, x)
              else
                raise Fail "this should not be possible"
             | _ => raise Fail "not a product t in Select1")
    | typeof_helper (g, A.Select2 t1) =
        (case t1
            of A.Pair (x, y) =>
              if typeof_helper(g, A.Pair (x, y)) = T.Product (typeof_helper (g, x), typeof_helper (g,y)) then
                typeof_helper (g, y)
              else
                raise Fail "this should not be possible"
             | _ => raise Fail "not a product t in Select1")
    | typeof_helper (g, A.Scope (s, t1, t2)) =
        let
          val newg = extend (g, s, typeof_helper(g, t1))
        in
          typeof_helper (newg, t2)
        end
    | typeof_helper (g, A.Variable x) = lookup (g, x)


(* typeof : term -> ty *)
(* raise an exception if a term is not well-typed, e.g., (not 0) *)
  fun typeof t = typeof_helper ([], t)

  fun check t = (t, typeof t)

end
