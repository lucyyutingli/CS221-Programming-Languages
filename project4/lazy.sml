structure Lazy : SMALL_STEP = struct

 structure U = ULC

  fun step (U.App (t1, t2)) =
        (case step t1
            of SOME t1' => SOME (U.App (t1', t2))
             | NONE =>
                (case t1
                  of (U.Abs (s, t1)) => SOME (Subst.subst s t2 (U.Abs (s, t1)))
                   | _ => NONE))
    | step _ = NONE

end
