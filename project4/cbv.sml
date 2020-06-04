structure CBV : SMALL_STEP = struct

(* val step : ULC.term -> ULC.term option *)

structure U = ULC


  fun step (U.App (U.Abs(x, t1), t2)) =
      if U.isValue t2 then
        SOME (Subst.subst x t2 (U.Abs (x, t1)))
      else
        NONE
    | step (U.App (t1, t2)) =
        (case step t1
          of SOME t1' => SOME (U.App (t1', t2))
           | NONE =>
              if U.isValue t1 then
                (case step t2
                    of SOME t2' => SOME (U.App (t1, t2'))
                     | NONE => NONE)
              else
                raise Fail "should not be possible")
    | step _ = NONE

end
