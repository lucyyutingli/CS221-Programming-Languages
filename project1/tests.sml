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





end
