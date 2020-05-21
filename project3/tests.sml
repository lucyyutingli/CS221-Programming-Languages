structure Tests = struct

  structure T = Token
  structure TT = Type
  structure S = Scan
  structure Sub = Subst
  structure A = AST
  structure P = Parse
  structure I = InternalAST
  structure D = Desugar
  structure E = Eval
  structure C = Compile
  structure Ty = Typecheck

  val expect = Check.expect

  fun scan () =
    let
      val _ = expect (S.scan "()", [T.LParen, T.RParen], "scan1")
      val _ = expect (S.scan "(if #t #f (not #t))", [T.LParen, T.If, T.HashT, T.HashF, T.LParen, T.Not, T.HashT, T.RParen, T.RParen], "scan2")
      val _ = expect (S.scan "(if#t#f(not    #t))", [T.LParen, T.If, T.HashT, T.HashF, T.LParen, T.Not, T.HashT, T.RParen, T.RParen], "scan3")
      val _ = expect (S.scan "and or pred isz = > +", [T.And, T.Or, T.Pred, T.Isz, T.Equals, T.GT, T.Plus], "scan4")
      val _ = expect (S.scan "1234", [T.NatConst 1234], "scan5")
      val _ = Check.exn (fn () => S.scan "$", "scan6")
      val _ = expect (S.scan ".x1", [T.Variable ".x1"], "scan7")
      val _ = expect (S.scan "._temp1", [T.Variable "._temp1"], "scan8")
    in
      print "scan tests done\n"
    end

  fun parse () =
    let
      val _ = expect (P.parse [T.NatConst 1234], A.NatConst 1234, "parse1")
      val _ = expect (P.parse [T.LParen, T.Plus, T.NatConst 1, T.NatConst 2, T.RParen], A.Plus (A.NatConst 1, A.NatConst 2), "parse2")
      val _ = expect (P.parse [T.LParen, T.Not, T.HashF, T.RParen], A.Not A.False, "parse3")
      val _ = expect (P.parse [T.LParen, T.If, T.LParen, T.GT, T.NatConst 1, T.NatConst 0, T.RParen, T.HashT, T.HashF, T.RParen],
		      A.If (A.GT (A.NatConst 1, A.NatConst 0), A.True, A.False),
		      "parse4")
      val _ = Check.exn (fn () => P.parse [T.RParen], "parse5")
    in
      print "parse tests done\n"
    end

  fun desugar () =
    let
      val _ = expect (D.desugar (A.NatConst 0), I.Zero, "desugar1")
      val _ = expect (D.desugar (A.NatConst 1), I.Succ I.Zero, "desugar2")
      val _ = expect (D.desugar (A.Not A.True), I.If (I.True, I.False, I.True), "desugar3")
      val _ = expect (D.desugar (A.And (A.True, A.False)), I.If (I.True, I.False, I.False), "desugar4")
      val _ = expect (D.desugar (A.Or (A.True, A.False)), I.If (I.True, I.True, I.False), "desugar5")
      val _ = expect (D.desugar (A.Isz (A.NatConst 0)), I.Eq (I.Zero, I.Zero), "desugar6")
      val _ = expect (D.desugar (A.Xor (A.True, A.False)), I.If (I.If (I.True, I.False, I.False),
                I.False, I.If (I.If (I.True, I.True, I.False), I.True, I.False)), "desugar7")
    in
      print "desugar tests done\n"
    end

  fun subst () =
    let
      val _ = expect (Sub.subst (".x", I.Zero, I.Variable ".x"), I.Zero, "subst1")
      val _ = expect (Sub.subst (".x", I.Zero, I.True), I.True, "subst2")
      val _ = expect (Sub.subst (".x", I.True, I.If (I.Variable ".x", I.Variable ".x", I.False)), I.If (I.True, I.True, I.False), "subst3")
      val _ = expect (Sub.subst (".lol", I.False, I.Pair (I.Variable ".lol", I.True)), I.Pair (I.False, I.True), "subst4")
      val _ = expect (Sub.subst (".x", I.Variable ".y", I.Variable ".x"), I.Variable ".y", "subst5")
    in
      print "substitute tests done\n"
    end


  fun eval () =
    let
      val _ = expect (E.step (I.Eq (I.If (I.True, I.Zero, I.Succ I.Zero), I.Succ I.Zero)), SOME (I.Eq (I.Zero, I.Succ I.Zero)), "step1")
      val _ = expect (E.steps (I.Eq (I.If (I.True, I.Zero, I.Succ I.Zero), I.Succ I.Zero)),
		      [I.Eq (I.If (I.True, I.Zero, I.Succ I.Zero), I.Succ I.Zero), I.Eq (I.Zero, I.Succ I.Zero), I.False],
		      "steps1")
      val _ = expect (E.eval (I.Eq (I.If (I.True, I.Zero, I.Succ I.Zero), I.Succ I.Zero)), E.Value I.False, "eval1")
      val _ = expect (E.step (I.Succ I.True), NONE, "step2")
      val _ = expect (E.steps (I.Succ I.True), [I.Succ I.True], "steps2")
      val _ = expect (E.eval (I.Succ I.True), E.Stuck (I.Succ I.True), "eval2")
      val _ = expect (E.step (I.Pair (I.True, I.False)), NONE, "step pair")
      val _ = expect (E.step (I.Select1 (I.Pair (I.True, I.False))), SOME I.True, "step select1")
      val _ = expect (E.step (I.Pair (I.Pred (I.Succ I.Zero), I.False)), SOME (I.Pair (I.Zero, I.False)), "step pair const")
      val _ = expect (E.step (I.Select2 (I.Pair (I.False, I.True))), SOME I.True, "step select2")
      val _ = expect (E.step (I.Scope (".x", I.Zero, I.Plus (I.Variable ".x", I.Zero))), SOME (I.Plus (I.Zero, I.Zero)), "isz scope")
      val _ = expect (E.step (I.Pair (I.False, I.If (I.True, I.True, I.False))), SOME (I.Pair (I.False, I.True)), "pair step1")
      val _ = expect (E.step (I.Variable ".y"), NONE, "step variable")
      val _ = expect (E.step (I.Scope (".x", I.True, (I.Scope (".x", I.Zero, I.Variable ".x")))), SOME (I.Scope (".x", I.Zero, I.Variable ".x")), "nested scope")
    in
      print "eval tests done\n"
    end

  fun typecheck () =
    let
      val _ = expect (Ty.typeof (A.Unit), TT.Unit, "typecheck Unit")
      val _ = expect (Ty.typeof (A.Or (A.True, A.False)), TT.Bool, "typecheck bool of Or")
      val _ = expect (Ty.typeof (A.Plus (A.NatConst 3, A.NatConst 4)), TT.Nat, "typecheck nat of plus")
      val _ = expect (Ty.typeof (A.Pair (A.True, A.False)), TT.Product (TT.Bool, TT.Bool), "typecheck bool of pair")
    in
      print "typecheck tests done"
    end


  fun compile () =
    let
      val _ = expect (C.evalTyped "0", (I.Zero, Type.Nat), "C.eval1")
      val _ = expect (C.evalUntyped "0", E.Value I.Zero, "C.eval2")
    in
      print "compile tests done\n"
    end

  fun run () =
    let
      val _ = scan ()
      val _ = parse ()
      val _ = desugar ()
      val _ = eval ()
      val _ = compile ()
      val _ = subst ()
      val _ = typecheck ()
    in
      print "tests done\n"
    end

  val _ = run ()

end
