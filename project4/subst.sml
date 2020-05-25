structure Subst : sig

  val fv    : ULC.term -> VarSet.set

  val subst : string -> ULC.term -> ULC.term -> ULC.term
  (* NOTE: subst x s2 t1 means "rewrite x to s2 in t1" *)
  (* i.e. "subst x s2 t1" corresponds to "[x |-> s2] t1" in TaPL *)

end = struct

  structure U = ULC

  fun fv (U.Var x) = VarSet.singleton x
    | fv (U.App (t1, t2)) = VarSet.union (fv t1, fv t2)
    | fv (U.Abs (x, t1)) = VarSet.remove (x, fv t1)

  fun inFV (x, t) = VarSet.member (x, fv t)

  fun subst x t2 (U.Var y) = if x=y then t2 else U.Var y
    | subst x t2 (U.App (t11, t22)) = U.App (subst x t2 t11, subst x t2 t22)
    | subst x t2 (U.Abs (y, t1)) =
        if x = y then U.Abs (y, t1)
        else if inFV (y, t2) then subst x t2 (freshen (y, t1))
        else U.Abs (y, subst x t2 t1)
  and freshen (y, t1) = (fn y' => U.Abs (y', subst y (U.Var y') t1)) (Fresh.name ())

end

(*                                                                                                         
[x |-> s] (lam x . t1) = lam x . t1                                                                        
[x |-> s] (lam y . t1) = lam y . [x |-> s] t1   (y ne x AND y not in FV(s))                                
[x |-> s] (lam y . t1) = [x |-> s] (lam y' . [y |-> y'] t1)   (y ne x BUT y in FV(s), y' some fresh name)  
*)


