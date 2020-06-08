structure CBV : SMALL_STEP = struct

(* val step : ULC.term -> ULC.term option *)

structure U = ULC


  fun step (U.App (U.Abs(x, t1), U.Abs(x2, t2))) =
        SOME (Subst.subst x (U.Abs(x2,t2)) t1)
    | step (U.App (t1, t2)) =
        (case step t1
          of SOME t1' => SOME (U.App (t1', t2))
           | NONE =>
              (case step t2
                  of SOME t2' => SOME (U.App (t1, t2'))
                   | NONE => NONE))
    | step _ = NONE

end
