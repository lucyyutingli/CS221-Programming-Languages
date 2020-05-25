structure VarSet :> sig

  type set

  val empty     : set
  val singleton : string -> set

  val sizeof : set -> int

  val member : string * set -> bool
  val insert : string * set -> set
  val remove : string * set -> set
  val union  : set * set -> set

end = struct

  type set = string list

  val empty = []

  fun singleton x = [x]

  fun sizeof s = List.length s

  fun member (x, s) = Option.isSome (List.find (fn y => y=x) s)

  fun insert (x, s) = if member (x, s) then s else x::s

  fun remove (x, s) = List.filter (fn y => y<>x) s

  fun union (s1, s2) = List.foldl insert s2 s1

end
