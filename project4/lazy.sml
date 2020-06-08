structure Lazy : SMALL_STEP = struct

 structure U = ULC

  fun step (U.App (U.Abs(x, t1), t2)) =
        SOME (Subst.subst x t2 t1)
    | step (U.App (t1, t2)) =
        (case step t1
            of SOME t1' => SOME (U.App (t1', t2))
             | NONE => NONE)
    | step _ = NONE

end
