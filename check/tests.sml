structure Tests = struct

  structure F = Functions

  val _ = Check.expect (F.fact 0,   1, "fact 0")
  val _ = Check.expect (F.fact 1,   1, "fact 1")
  val _ = Check.expect (F.fact 5, 120, "fact 5")

  val _ = Check.expect (F.exclaim "Hello", "Hello!", "Hello")

  val _ = Check.assert (F.isSweet F.Mango, "mango")
  val _ = Check.assert (not (F.isSweet F.Lime), "lime")

end
