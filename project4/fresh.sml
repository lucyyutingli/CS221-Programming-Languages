structure Fresh : sig

  val name  : unit -> string
  val reset : unit -> unit
			  
end = struct

  fun ++ r =
    let
      val curr = !r
      val newval = curr+1
    in
      (r:=newval; newval)
    end

  val i = ref 0

  fun name () = "@" ^ Int.toString (++i)

  fun reset () = (i:=0)

end
