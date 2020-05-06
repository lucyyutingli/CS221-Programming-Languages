structure Eval : sig

  val step  : InternalAST.term -> InternalAST.term option
  val steps : InternalAST.term -> InternalAST.term list

  datatype normal_form
    = Value of InternalAST.term
    | Stuck of InternalAST.term

  val eval  : InternalAST.term -> normal_form

end = struct

  structure I = InternalAST

  fun step I.True = NONE
    | step I.False = NONE
    | step (I.If (I.True, x, y)) = SOME x
    | step (I.If (I.False, x, y)) = SOME y
    | step (I.Succ x) =
        if (I.isNumericValue x) = true then
          NONE
        else
          (case step x
              of SOME x' => SOME (I.Succ x')
              |  NONE => NONE)
    | step (I.Pred I.Zero) = SOME I.Zero
    | step (I.Pred (I.Succ x)) =
        if x = I.Zero then
          SOME I.Zero
        else
          (case step x
              of SOME x' => SOME (I.Pred (I.Succ x'))
              | NONE => NONE)
    | step (I.Pred x) =
        (case step x
            of SOME x' => SOME (I.Pred x')
            |  NONE => NONE)
            | step (I.If (a, x, y)) =
                (case step a
                    of SOME a' => SOME (I.If (a', x, y))
                    |  NONE => NONE)

    (** steps for Eq **)
    | step (I.Eq (I.Zero, I.Zero)) = SOME I.True
    | step (I.Eq (I.Zero, I.Succ x)) =
        (case step x
            of SOME x' => SOME (I.Eq (I.Zero, I.Succ x'))
             | NONE => SOME I.False)
    | step (I.Eq (I.Succ x, I.Zero)) =
        (case step x
            of SOME x' => SOME (I.Eq (I.Succ x', I.Zero))
             | NONE => SOME I.False)
    | step (I.Eq (I.Succ x, I.Succ y)) =
        (case step x
            of SOME x' =>
              (case step y
                  of SOME y' => SOME (I.Eq (I.Succ x', I.Succ y'))
                   | NONE => SOME (I.Eq (I.Succ x', I.Succ y)))
             | NONE =>
                (case step y
                    of SOME y' => SOME (I.Eq (I.Succ x, I.Succ y'))
                     | NONE => SOME (I.Eq (x, y))))
    | step (I.Eq (x,y)) =
        if (I.isNumericValue x) = true then
          (case step y
              of SOME y' => SOME (I.Eq (x, y'))
               | NONE => SOME (I.Eq (x,y)))
        else
          (case step x
              of SOME x' => SOME (I.Eq (x', y))
               | NONE => SOME (I.Eq (x, y)))

    (** steps for GT **)
    | step (I.GT (I.Zero, x)) =
        (case step x
            of SOME x' => SOME (I.GT (I.Zero, x'))
             | NONE => SOME I.False)
    | step (I.GT (I.Succ x, I.Zero)) =
        (case step x
            of SOME x' => SOME (I.GT (I.Succ x', I.Zero))
             | NONE => SOME I.True)
    | step (I.GT (I.Succ x, I.Succ y)) =
        (case step x
            of SOME x' =>
              (case step y
                  of SOME y' => SOME (I.GT (I.Succ x', I.Succ y'))
                   | NONE => SOME (I.GT (I.Succ x', I.Succ y)))
             | NONE =>
                (case step y
                    of SOME y' => SOME (I.GT (I.Succ x, I.Succ y'))
                     | NONE => SOME (I.GT (x, y))))
    | step (I.GT (x,y)) =
        if (I.isNumericValue x) = true then
          (case step y
              of SOME y' => SOME (I.GT (x, y'))
               | NONE => SOME (I.GT (x,y)))
        else
          (case step x
              of SOME x' => SOME (I.GT (x', y))
               | NONE => SOME (I.GT (x, y)))

    (** steps for Plus **)
    | step (I.Plus (I.Zero, x)) =
        if (I.isNumericValue x) = true then
          SOME x
        else
          (case step x
              of SOME x' => SOME (I.Plus (I.Zero, x'))
               | NONE => NONE)
    | step (I.Plus (I.Succ x, y)) =
        (case step x
            of SOME x' => SOME (I.Plus (I.Succ x', y))
             | NONE =>
                (case step y
                  of SOME y' => SOME (I.Plus (x, y'))
                   | NONE => SOME (I.Plus (x,y))))
    | step (I.Plus (x,y)) =
        if (I.isNumericValue x) = true then
          (case step y
              of SOME y' => SOME (I.Plus (x, y'))
               | NONE => SOME (I.Plus (x,y)))
        else
          (case step x
              of SOME x' => SOME (I.Plus (x', y))
               | NONE => SOME (I.Plus (x, y)))
    | step _ = NONE


  fun steps t =
    (case step t
         of SOME t' => t :: steps t'
          | NONE => [t])

  datatype normal_form
    = Value of I.term
    | Stuck of I.term

  fun eval t =
    (case step t
        of NONE =>
          if (I.isValue t) = true then
            Value t
          else
            Stuck t
          | SOME t' => eval t')

end
