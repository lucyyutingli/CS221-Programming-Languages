structure Analysis : sig

  val size : NB.term -> int
  val indexedSet : int -> TermSet.set

end  = struct

  fun permutation (head::tail, head1::nil, head2::nil) =
      let
        val savelist = head::tail
      in
        TermSet.insert(NB.If (head, head1, head2), permutation(tail, savelist, savelist))
      end
    | permutation(head::tail, head1::tail1, head2::nil) =
      let
        val savelist = head::tail
      in
        TermSet.insert(NB.If (head, head1, head2), permutation(head::tail, tail1, savelist))
      end
    | permutation (head::tail,head1::tail1,head2::tail2) =
      let
        val savelist = head2::tail2
      in
        TermSet.insert(NB.If (head, head1, head2),
          permutation(head::tail, head1::tail1, tail2))
      end
    | permutation _ = TermSet.empty



  fun size NB.True = 1
    | size NB.False = 1
    | size NB.Zero = 1
    | size (NB.Succ t) = (size t) + 1
    | size (NB.Pred t) = (size t) + 1
    | size (NB.IsZero t) = (size t) + 1
    | size (NB.If (x,y,z)) = (size x) + (size y) + (size z) + 1

  fun indexedSet 0 = TermSet.empty
    | indexedSet 1 = TermSet.insert(NB.Zero, (TermSet.insert
      (NB.True, TermSet.singleton(NB.False))))
    | indexedSet 2 = permutation([NB.True, NB.False, NB.Zero],
      [NB.True, NB.False, NB.Zero], [NB.True, NB.False, NB.Zero])

end
