structure Inline : sig

  val inline_help : string list * SULC.program -> SULC.term
  val inline : SULC.program -> SULC.term

end = struct

  structure S = SULC

  fun same_abbv ([], abbv) = false
    | same_abbv (x::xs, abbv) =
    if x = abbv then
      true
    else
      same_abbv(xs, abbv)


  fun inline_help ([], S.Abbr (abbv, term, restProg)) =
        S.App (S.Abs (abbv, inline_help ([] @ [abbv], restProg)), term)
    | inline_help (list, S.Abbr (abbv, term, restProg)) =
        if same_abbv (list, abbv) then
          raise Fail "cannot use the same abbv name twice"
        else
          S.App (S.Abs (abbv, inline_help (list @ [abbv], restProg)), term)
    | inline_help ([], S.Term t) =
        t
    | inline_help (list, S.Term t) =
        t


  fun inline (S.Abbr (str, t, p)) =
        inline_help ([], S.Abbr (str, t, p))
    | inline (S.Term t1) = t1

end
