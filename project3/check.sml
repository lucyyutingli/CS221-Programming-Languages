structure Check : sig

  (* check if two items are equal by built-in polymorphic equality *)
  val expect : ''a * ''a * string -> unit

  (* check if given boolean is true *)
  val assert : bool * string -> unit

  (* check if two items are equal by equality function *)
  val expectBy : ('a * 'a -> bool) * 'a * 'a * string -> unit

  (* check if the first item is among the list by built-in polymorphic equality *)
  val among : ''a * (''a list) * string -> unit

  (* check if the first item is among the list by equality function *)
  val amongBy : ('a * 'a -> bool) * 'a * ('a list) * string -> unit

  (* check if two floating-point values are within epsilon of another *)
  val within : real * real * real * string -> unit

  (* check if given delayed computation raises an exception *)
  val exn : (unit -> 'a) * string -> unit

  (* given a toString function, look at two results *)
  (* - this is useful for values where algorithmic equality *)
  (*   is either difficult or impossible *)
  val eyeball : ('a -> string) * 'a * 'a * string -> unit

end = struct

  fun msg s m = "Check." ^ s ^ " failure: " ^ m ^ "\n"

  fun assert (b, m) =
    if b then () else raise Fail (msg "assert" m)

  fun expect (x, y, m) =
    if x=y then () else raise Fail (msg "expect" m)

  fun expectBy (eq, x, y, m) =
    if eq(x,y) then () else raise Fail (msg "expectBy" m)

  fun among (x, ys, m) =
    let
      fun lp [] = raise Fail (msg "among" m)
        | lp (y::ys) = if x=y then () else lp ys
    in
      lp ys
    end

  fun amongBy (eq, x, ys, m) =
    let
      fun lp [] = raise Fail (msg "amongBy" m)
        | lp (y::ys) = if eq(x,y) then () else lp ys
    in
      lp ys
    end

  fun within (eps:real, x, y, m) =
    if abs(x-y)<=eps then () else raise Fail (msg "within" m)

  fun exn (compute, m) =
    let
      val x = SOME (compute ()) handle _ => NONE
    in
     (case x
        of NONE => ()
	|  SOME _ => raise Fail (msg "exn" m))
    end

  fun eyeball (tos, x, y, m) =
    let
      fun pr s = (TextIO.print s; TextIO.print "\n")
      val _ = pr ("eyeball test (" ^ m ^ "):")
      val _ = pr (tos x)
      val _ = pr (tos y)
    in
      pr ""
    end

end

