structure Eval : sig

  datatype normal
    = Value of NB.term
    | Stuck of NB.term

  val step  : NB.term -> NB.term option
  val steps : NB.term -> NB.term list
  val eval  : NB.term -> normal

end = struct

  datatype normal
    = Value of NB.term
    | Stuck of NB.term

  fun step NB.True = NONE
    | step NB.False = NONE
    | step (NB.IsZero NB.Zero) = SOME NB.True
    | step (NB.If (NB.True, x, y)) = SOME x
    | step (NB.If (NB.False, x, y)) = SOME y
    | step (NB.Succ x) =
        if (NB.isNumericValue x) = true then
          (case step x
              of SOME x' => SOME (NB.Succ x')
              |  NONE => raise Fail "NB.Succ something is wrong")
        else
          NONE
    | step (NB.IsZero (NB.Succ x)) = SOME NB.False
    | step (NB.IsZero x) =
      (case step x
          of SOME x' => SOME (NB.IsZero x')
          |  NONE => raise Fail "IsZero, something is wrong")
    | step (NB.Pred NB.Zero) = SOME NB.Zero
    | step (NB.Pred (NB.Succ x)) =
        if x = NB.Zero then
          SOME NB.Zero
        else
          (case step x
              of SOME x' => SOME (NB.Pred (NB.Succ x'))
              | NONE => raise Fail "NB.Pred NB.Xucc something is wrong")
    | step (NB.Pred x) =
        (case step x
            of SOME x' => SOME (NB.Pred x')
            |  NONE => raise Fail "NB.Pred x error")
            | step (NB.If (a, x, y)) =
                (case step a
                    of SOME a' => SOME (NB.If (a', x, y))
                    |  NONE => raise Fail "step, something is wrong")
    | step _ = NONE



  fun steps t =
    (case step t
         of SOME t' => t :: steps t'
          | NONE => [t])

  fun eval t =
    (case step t
        of NONE =>
          if (NB.isValue t) = true then
            Value t
          else
            Stuck t
          | SOME t' => eval t')

end
