structure Tests = struct

  structure T = Token
  structure S = Scan
  structure A = AST
  structure P = Parse
  structure I = InternalAST
  structure D = Desugar
  structure E = Eval
  structure C = Compile
  
  val expect = Check.expect

  fun scan () =
    let
      val _ = expect (S.scan "()", [T.LParen, T.RParen], "scan1")
      val _ = expect (S.scan "(if #t #f (not #t))", [T.LParen, T.If, T.HashT, T.HashF, T.LParen, T.Not, T.HashT, T.RParen, T.RParen], "scan2")
      val _ = expect (S.scan "(if#t#f(not    #t))", [T.LParen, T.If, T.HashT, T.HashF, T.LParen, T.Not, T.HashT, T.RParen, T.RParen], "scan3")
      val _ = expect (S.scan "and or pred isz = > +", [T.And, T.Or, T.Pred, T.Isz, T.Equals, T.GT, T.Plus], "scan4")		     
      val _ = expect (S.scan "1234", [T.NatConst 1234], "scan5")
      val _ = Check.exn (fn () => S.scan "$", "scan6")
    in
      "scan tests done"
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
      "parse tests done"
    end

  fun desugar () =
    let
      val _ = expect (D.desugar (A.NatConst 0), I.Zero, "desugar1")
      val _ = expect (D.desugar (A.NatConst 1), I.Succ I.Zero, "desugar2")
      val _ = expect (D.desugar (A.Not A.True), I.If (I.True, I.False, I.True), "desugar3")
      val _ = expect (D.desugar (A.And (A.True, A.False)), I.If (I.True, I.False, I.False), "desugar4")
      val _ = expect (D.desugar (A.Or (A.True, A.False)), I.If (I.True, I.True, I.False), "desugar5")
      val _ = expect (D.desugar (A.Isz (A.NatConst 0)), I.Eq (I.Zero, I.Zero), "desugar6")
    in
      "desugar tests done"
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
    in
      "eval tests done"
    end

  fun compile () =
    let
      val _ = expect (C.evalTyped "0", (I.Zero, Type.Nat), "C.eval1")
      val _ = expect (C.evalUntyped "0", E.Value I.Zero, "C.eval2")
    in
      "compile tests done"
    end
	
  fun run () =
    let
      val _ = scan ()
      val _ = parse ()
      val _ = desugar ()
      val _ = eval ()
      val _ = compile ()
    in
      "tests done"
    end
 
end
