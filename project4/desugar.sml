structure Desugar : sig

  val desugar : SULC.term -> ULC.term
  val nathelper : ULC.term * int -> ULC.term

end = struct

  structure S = SULC
  structure U = ULC

  fun nathelper (input, num) =
    if num = 0 then
      input
    else
      U.App (U.Var "s", nathelper (input, num - 1))


  fun desugar (S.Var (name)) = U.Var (name)
    | desugar (S.Abs (s, t)) = U.Abs (s, desugar t)
    | desugar (S.App (s, t)) = U.App (desugar s, desugar t)
    | desugar (S.Nat (n)) =
        if n = 0 then
          U.Abs ("s", U.Abs ("z", U.Var "z"))
        else
          U.Abs ("s", U.Abs ("z", nathelper (U.App (U.Var "s", U.Var "z"), n-1)))
    | desugar (S.ID (str)) = U.Abs (str, U.Var str)
    | desugar (S.Pair (t1, t2)) =
        U.App (U.App (U.Abs ("f", U.Abs ("s", U.Abs ("b",
          U.App (U.Var "b", U.App(U.Var "f", U.Var "s"))))),
            desugar (t1)), desugar (t2))
    | desugar (S.Select1 t1) =
        (case t1
          of S.Pair (x, y) => U.App (U.Abs ("p", U.App (U.Var "p", U.Abs ("t", U.Abs ("f", U.Var "t")))),
                       U.Abs ("b", U.App (U.Var "b", U.App (desugar x, desugar y))))
           | _ => raise Fail "can't select of something that's not a pair")
    | desugar (S.Select2 t1) =
        (case t1
          of S.Pair (x, y) => U.App (U.Abs ("p", U.App (U.Var "p", U.Abs ("t", U.Abs ("f", U.Var "f")))),
                       U.Abs ("b", U.App (U.Var "b", U.App (desugar x, desugar y))))
           | _ => raise Fail "can't select of something that's not a pair")

end
