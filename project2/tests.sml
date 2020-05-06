structure Tests = struct

  structure I = InternalAST
  structure A = AST
  structure E = Eval
  structure T = Token


  val expect = Check.expect
  val eval = Compile.eval
  val scan = Scan.scan
  val tos = A.tos
  val parse = Parse.parse
  val desugar = Desugar.desugar

  fun testScan () =
    let
      val _ = expect (scan "(if #t 1 0)", [T.LParen, T.If, T.HashT, T.NatConst 1,
        T.NatConst 0, T.RParen], "scan1")
      val _ = expect (scan "(= 0 0)", [T.LParen, T.Equals, T.NatConst 0, T.NatConst 0,
        T.RParen], "scan2")
      val _ = expect (scan "(not 3)", [T.LParen, T.Not, T.NatConst 3, T.RParen],
        "scan3")
      val _ = expect (scan "(+ 4 0)", [T.LParen, T.Plus, T.NatConst 4, T.NatConst 0, T.RParen],
        "scan4")
      val _ = expect (scan "(= 100 0)", [T.LParen, T.Equals, T.NatConst 100, T.NatConst 0, T.RParen],
        "scan5")
    in
      print "scan tests done\n"
    end

  fun testTosAST () =
    let
      val _ = expect (tos (A.Pred (A.NatConst 0)), "(pred 0)", "tos1")
      val _ = expect (tos (A.If (A.True, A.False, (A.NatConst 0))), "(if #t #f 0)", "tos2")
      val _ = expect (tos (A.Plus ((A.NatConst 3), (A.NatConst 4))), "(+ 3 4)", "tos3")
      val _ = expect (tos (A.Plus ((A.If (A.True, A.False, (A.NatConst 2))), (A.NatConst 3))),
        "(+ (if #t #f 2) 3)", "tos4")
    in
      print "tos tests done\n"
    end

  fun testParse () =
    let
      val _ = expect (parse ([T.LParen, T.If, T.HashT, T.HashF, (T.NatConst 0), T.RParen]),
        A.If (A.True, A.False, (A.NatConst 0)), "parse1")
      val _ = expect (parse ([T.HashT]), A.True, "parse2")
      val _ = expect (parse ([T.LParen, T.Not, T.HashT, T.RParen]), (A.Not A.True), "parse3")
      val _ = expect (parse ([T.LParen, T.If, T.HashT, T.HashF, T.LParen, T.If, T.HashF,
        T.HashT, T.HashF, T.RParen, T.RParen]), (A.If (A.True, A.False, A.If (A.False, A.True, A.False))),
        "parse4")
    in
      print "parse tests done\n"
    end


  fun testIAST () =
    let
      val _ = expect (I.isNumericValue I.Zero, true,
        "isNumeric test 0")
      val _ = expect (I.isNumericValue (I.Succ I.Zero), true,
        "isNumeric succ(0) test")
      val _ = expect (I.isNumericValue (I.Pred I.Zero), false,
        "isNumeric pred(0) test")
      val _ = expect (I.isNumericValue I.True, false,
        "isNumeric True test")
      val _ = expect (I.isNumericValue I.False, false,
        "isNumeric False test")
      val _ = expect (I.isNumericValue (I.Succ (I.Pred I.Zero)), false,
        "isNumeric complex test")
      val _ = expect (I.isNumericValue (I.Succ (I.Succ (I.Succ I.Zero))),
        true, "isNumeric succ only test")
      val _ = expect(I.isValue I.True, true, "isValue True test")
      val _ = expect(I.isValue I.False, true, "isValue False test")
      val _ = expect(I.isValue (I.Succ I.Zero), true, "isValue succ zero test")
      val _ = expect(I.isValue (I.Pred I.Zero), false, "isValue pred zero test")

      val _ = expect(I.tos (I.Succ I.Zero), "(succ 0)", "tos succ zero test")
      val _ = expect(I.tos (I.Pred I.Zero), "(pred 0)", "tos pred zero test")
      val _ = expect(I.tos I.True, "#t", "tos true test")
      val _ = expect(I.tos I.False, "#f", "tos false test")
      val _ = expect(I.tos (I.If (I.True, I.Zero, I.Succ I.Zero)),"(if #t 0 (succ 0))",
        "tos If complex test")
    in
      print "InternalAST tests done\n"
    end

  fun testDesugar () =
    let
      val _ = expect (desugar (A.Or (A.True, A.False)), I.If (I.True, I.True, I.False), "desugar1")
      val _ = expect (desugar (A.Not A.False), I.If (I.False, I.False, I.True), "desugar2")
      val _ = expect (desugar (A.NatConst 3), I.Succ (I.Succ (I.Succ I.Zero)), "desugar3")
      val _ = expect (desugar (A.And (A.True, A.False)), I.If (I.True, I.False, I.False), "desugar4")
      val _ = expect (desugar (A.If (A.If (A.True, A.False, A.False), A.True, A.False)),
        I.If (I.If (I.True, I.False, I.False), I.True, I.False), "desugar")
      in
        print "desugar tests done\n"
      end

  fun testCompile () =
    let
      val _ = expect (eval "(if #t 1 0)", E.Value (I.Succ I.Zero), "compile test 0")
      val _ = print "compile tests passed!\n"
    in
      ()
    end



  fun run () =
    let
      val _ = testScan ()
      val _ = testTosAST ()
      val _ = testParse ()
      val _ = testIAST ()
      val _ = testDesugar ()
      val _ = testCompile ()
    in
      print "tests done\n"
    end

  val _ = run ()


end
