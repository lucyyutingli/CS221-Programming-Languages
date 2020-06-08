structure Inline : sig

  val inline : SULC.program -> SULC.term

end = struct

  structure S = SULC

  fun abbv_exists ([], abbv) = false
    | abbv_exists ((abbv1, t)::xs, abbv) =
    if abbv1 = abbv then
      true
    else
      abbv_exists (xs, abbv)


  fun expand_term ([], s) = raise Fail "empty list"
    | expand_term ((abbv, t) :: tail, s) =
        if (abbv = s) then
          t
        else
          expand_term (tail,s)

  fun replace (abbvlist, S.Var s) =
        if abbv_exists (abbvlist, s) then
          expand_term (abbvlist, s)
        else
          S.Var s
    | replace (abbvlist, S.Abs (s, t)) = S.Abs (s, replace (abbvlist, t))
    | replace (abbvlist, S.App (t1, t2)) = S.App (replace(abbvlist, t1), replace(abbvlist, t2))
    | replace (abbvlist, S.Nat (n)) = S.Nat n
    | replace (abbvlist, S.ID (s)) = S.ID s
    | replace (abbvlist, S.Pair (t1, t2)) = S.Pair (replace(abbvlist, t1), replace(abbvlist, t2))
    | replace (abbvlist, S.Select1 (t1)) = S.Select1 (replace (abbvlist,t1))
    | replace (abbvlist, S.Select2 (t2)) = S.Select2 (replace (abbvlist,t2))


  fun inline_help (list, S.Abbr (abbv, term, restProg)) =
        if abbv_exists (list, abbv) then
          raise Fail "cannot use the same abbv name twice"
        else
          inline_help (list @ [(abbv, replace(list, term))], restProg)
    | inline_help (list, S.Term t) =
        replace (list, t)


  fun inline (S.Abbr (abbv, term, restProg)) =
        inline_help ([], S.Abbr (abbv, term, restProg))
    | inline (S.Term t1) = inline_help ([], S.Term t1)

end
