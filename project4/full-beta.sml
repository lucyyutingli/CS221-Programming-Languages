structure FullBeta : SMALL_STEP = struct

  structure U = ULC

  fun isNA (U.Var name) = true
    | isNA (U.App (t1, t2)) = true
    | isNA _ = false

  fun isNF (U.Abs(s, t)) =
        if isNF t then
          true
        else
          false
    | isNF t1 =
        if isNANF t1 then
          true
        else
          false

  and isNANF (U.Var name) = true
    | isNANF (U.App (t1, t2)) =
        if isNANF t1 then
          if isNF t2 then
            true
          else
            false
        else
          false
    | isNANF _ = false

  fun step (U.App (U.Abs (s, t1), t2)) = SOME (Subst.subst s t2 t1)
    | step (U.App (t1, t2)) =
        if isNA t1 then
          (case step t1
              of SOME t1' => SOME (U.App (t1', t2))
               | NONE => 
                    (case step t2
                        of SOME t2' => SOME (U.App (t1, t2'))
                        | NONE => NONE))
        else if isNANF t1 then
            (case step t2
                of SOME t2' => SOME (U.App (t1, t2'))
                 | NONE => NONE)
        else 
            NONE
    | step (U.Abs (s, t)) =
        (case step t
            of SOME t' => SOME (U.Abs (s, t'))
             | NONE => NONE)
    | step _ = NONE



end
