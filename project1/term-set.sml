structure TermSet :> sig

  type set

  val empty : set
  val singleton : NB.term -> set
  val size : set -> int

  val insert : NB.term * set -> set
  val union : set * set -> set
  val toList : set -> NB.term list

end = struct

  type set = NB.term list

  val empty = nil

  fun singleton x = [x]

  fun size x = List.length x

  fun insert (x, list) = x :: list

  fun union (nil, list1) = list1
    | union (list, nil) = list
    | union (head :: tail, list2) =
        if (List.exists (fn head2 => if head2 = head then true else false) list2)
          = true then
              union(tail, list2)
        else
          union(tail, head :: list2)

  fun toList list = list

  val _ = Check.expect(singleton (NB.Succ NB.Zero), [NB.Succ NB.Zero], "NB.Succ zero singleton test")
  val _ = Check.expect(singleton (NB.True), [NB.True], "NB.True singleton test")
  val _ = Check.expect(singleton (NB.False), [NB.False], "NB.False singleton test")
  val _ = Check.expect(singleton (NB.Pred NB.Zero), [NB.Pred NB.Zero], "NB.Pred zero singleton test")

  val _ = Check.expect(size [NB.Zero, NB.True, NB.False], 3, "3 size test")
  val _ = Check.expect(size [NB.Succ NB.Zero, NB.Pred NB.Zero, NB.True, NB.False, NB.True], 5,
            "5 size test")
  val _ = Check.expect(size [], 0, "0 size test")

  val _ = Check.expect(insert (NB.True, [NB.False, NB.True, NB.Succ NB.Zero]), [NB.True, NB.False, NB.True,
            NB.Succ NB.Zero], "insert true test")
  val _ = Check.expect(insert (NB.False, [NB.True, NB.Zero]), [NB.False, NB.True, NB.Zero],
            "insert false test")
  val _ = Check.expect(insert (NB.Zero, [NB.True, NB.Pred NB.Zero, NB.False]), [NB.Zero,
            NB.True, NB.Pred NB.Zero, NB.False], "insert zero test")

  val _ = Check.expect(union([NB.Zero, NB.False, NB.True], [NB.Succ NB.Zero, NB.Pred NB.Zero]),
            [NB.True, NB.False, NB.Zero, NB.Succ NB.Zero, NB.Pred NB.Zero], "union unique test")
  val _ = Check.expect(union([NB.Zero, NB.False, NB.True], [NB.Zero, NB.Succ NB.Zero, NB.Pred NB.Zero]),
            [NB.True, NB.False, NB.Zero, NB.Succ NB.Zero, NB.Pred NB.Zero], "not")

  val _ = Check.expect(toList ([NB.Zero, NB.False]), [NB.Zero, NB.False], "toList zero false test")
  val _ = Check.expect(toList ([NB.Succ NB.Zero, NB.True, NB.False]), [NB.Succ NB.Zero, NB.True, NB.False],
            "toList succ zero, true, false")


end
