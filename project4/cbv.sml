structure CBV : SMALL_STEP = struct

(* val step : ULC.term -> ULC.term option *)

structure U = ULC


  fun step App (Abs(x, t1), t2) = Subst.subst X t2 Abs (x, t1)
    | step (App (t1, t2)) =
        (case step t1
          of t1' => SOME (App (t1', t2))
           | NONE =>
              if U.isValue t1 then
                (case step t2
                    of t2' => SOME (App t1, t2')
                     | NONE => NONE)
              else
                raise Fail "should not be possible")
    | step _ = NONE

end
