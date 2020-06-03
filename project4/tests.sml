structure Tests = struct

  structure I = Inline
  structure S = SULC
  structure U = ULC
  structure D = Desugar

  val expect = Check.expect

  fun inline () =
    let
      val _ = expect(I.inline (S.Abbr ("_one", S.Nat 1, S.Abbr ("_two", S.Nat 2, S.Term (S.Var "_two")))),
              S.App (S.Abs ("_one", S.App (S.Abs ("_two", S.Var "_two"), S.Nat 2)), S.Nat 1),"inline 1")
    in
      print "inline tests done\n"
    end


  fun desugar () =
    let
      val _ = expect (D.desugar (S.Pair (S.Var "x", S.Var "y")),
              U.App (U.App (U.Abs ("f", U.Abs ("s", U.Abs ("b", U.App (U.Var "b", U.App (U.Var "f", U.Var "s"))))), U.Var "x"), U.Var "y"),
              "desugar 1")
      val _ = expect (D.desugar (S.Nat 4),
              U.Abs ("s", U.Abs ("z", U.App (U.Var "s", U.App (U.Var "s",U.App (U.Var "s", U.App (U.Var "s", U.Var "z")))))),
      "desugar 2")
    in
      print "desugar tests done"
    end


  fun run () =
    let
      val _ = inline ()
      val _ = desugar ()
    in
      print "tests done\n"
    end

  val _ = run ()

end
