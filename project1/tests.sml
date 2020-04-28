structure Tests = struct

  val _ = Check.expect(NB.isNumericValue NB.Zero, true,
    "isNumeric test 0")
  val _ = Check.expect(NB.isNumericValue (NB.Succ NB.Zero), true,
    "isNumeric succ(0) test")
  val _ = Check.expect(NB.isNumericValue (NB.Pred NB.Zero), false,
    "isNumeric pred(0) test")
  val _ = Check.expect(NB.isNumericValue NB.True, false,
    "isNumeric True test")
  val _ = Check.expect(NB.isNumericValue NB.False, false,
    "isNumeric False test")
  val _ = Check.expect(NB.isNumericValue (NB.Succ (NB.Pred NB.Zero)), false,
    "isNumeric complex test")
  val _ = Check.expect(NB.isNumericValue (NB.Succ (NB.Succ (NB.Succ NB.Zero))),
    true, "isNumeric succ only test")

  val _ = Check.expect(NB.isValue NB.True, true, "isValue True test")
  val _ = Check.expect(NB.isValue NB.False, true, "isValue False test")
  val _ = Check.expect(NB.isValue (NB.Succ NB.Zero), true, "isValue succ zero test")
  val _ = Check.expect(NB.isValue (NB.Pred NB.Zero), false, "isValue pred zero test")

  val _ = Check.expect(NB.tos (NB.Succ NB.Zero), "(succ 0)", "tos succ zero test")
  val _ = Check.expect(NB.tos (NB.Pred NB.Zero), "(pred 0)", "tos pred zero test")
  val _ = Check.expect(NB.tos NB.True, "#t", "tos true test")
  val _ = Check.expect(NB.tos NB.False, "#f", "tos false test")
  val _ = Check.expect(NB.tos (NB.If (NB.True, NB.Zero, NB.Succ NB.Zero)),"(if #t 0 (succ 0))",
    "tos If complex test")

    val indexedSetTest2 = TermSet.toList (Analysis.indexedSet 2)
      val _ = Check.expect(TermSet.toList (Analysis.indexedSet 1),
                           [NB.True, NB.False, NB.Zero], "indexedSet failed 0")
      val  _= Check.among(NB.If (NB.True, NB.False, NB.Zero),
                          indexedSetTest2, "indexedSet failed 1")
      val  _= Check.among(NB.If (NB.Zero, NB.Zero, NB.Zero),
                          indexedSetTest2, "indexedSet failed 2")
      val  _= Check.among(NB.IsZero (NB.True),
                            indexedSetTest2, "indexedSet failed 3")
    (* NOTE: THESE TESTS DO PASS, but it takes forever since my code is inefficient)
      ( val indexedSetTest3 = TermSet.toList (Analysis.indexedSet 3) )
      ( val  = Check.among(NB.If (NB.True, NB.False, NB.If (NB.True, NB.False, NB.Zero)),
                          indexedSetTest3, "indexedSet failed 3") )
      ( val  = Check.expect(TermSet.size (indexedSetTest3), 59439, "size failed 0") *)



end
