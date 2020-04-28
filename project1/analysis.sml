structure Analysis : sig

  val size : NB.term -> int
  val indexedSet : int -> TermSet.set

end  = struct

  fun permutation (head::tail, head1::nil, head2::nil, head3::tail3) =
      let
        val savelist = head3::tail3
      in
        TermSet.insert(NB.If (head, head1, head2), permutation(tail, savelist, savelist, head3::tail3))
      end
    | permutation(head::tail, head1::tail1, head2::nil, head3::tail3) =
      let
        val savelist = head3::tail3
      in
        TermSet.insert(NB.If (head, head1, head2), permutation(head::tail, tail1, savelist, head3::tail3))
      end
    | permutation (head::tail,head1::tail1,head2::tail2, head3::tail3) =
      let
        val savelist = head3::tail3
      in
        TermSet.insert(NB.If (head, head1, head2),
          permutation(head::tail, head1::tail1, tail2, head3::tail3))
      end
    | permutation _ = TermSet.empty


    fun otherinputs x (head::nil) = TermSet.singleton (x head)
      | otherinputs x (head::tail) = TermSet.insert(x head, otherinputs x tail)


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
    | indexedSet n =
      let
        val prev = indexedSet(n-1)
        val previous = TermSet.toList(prev)
        val spicycurry = permutation(previous, previous, previous, previous)
      in
        TermSet.union(prev, TermSet.union (otherinputs NB.IsZero previous,
          TermSet.union(otherinputs NB.Succ previous, TermSet.union (otherinputs NB.Pred previous, spicycurry))))
      end


end
