structure HW1 : sig

  type rational = int * int

  datatype point = Point of real * real

  datatype shape
    = Circle of point * real
    | Triangle of point * point * point
    | Square of {upper_left_corner:point, rotation:real, side:real}

  type color = {r : int, g : int, b : int}

  val reduce : rational -> rational
  val midpoint : point * point -> point
  val perimeter : shape -> real
  val meanColor : color list -> color
  val selectionSort : int list -> int list

end = struct

  type rational = int * int

  datatype point = Point of real * real

  datatype shape
    = Circle of point * real
    | Triangle of point * point * point
    | Square of {upper_left_corner:point, rotation:real, side:real}

  type color = {r : int, g : int, b : int}

  (* ... your code goes here ... *)


  fun commondivisor(num, 0) = num
    | commondivisor (num, denom) = commondivisor(denom, num mod denom)

  fun reduce (num, denom) =
  let
    val gcd = commondivisor(num,denom)
    val newnum = num div gcd
    val newdenom = denom div gcd
  in
    if (num = denom) then
      (1,1)
    else
      (newnum, newdenom)
    end


  fun midpoint(Point(x,y), Point(w, z)) =
    ((x+w)/2.0, (y+z)/2.0)


  fun perimeter(shape) =
      (case shape
        of Circle(Point(x,y), radius) =>
          let
            val perimeter = 2.0*Math.pi*radius
          in
            perimeter
          end
        | Triangle(Point(a,b), Point(c,d), Point(e,f)) =>
          let
            val length1 = Math.sqrt((c-a)*(c-a) + (d-b)*(d-b))
            val length2 = Math.sqrt((e-c)*(e-c) + (d-f)*(d-f))
            val length3 = Math.sqrt((e-a)*(e-a) + (d-f)*(d-f))
          in
            length1 + length2 + length3
          end
        | Square{side = side, ...} =>
            side*4.0)

  fun sumColor(nil) = raise Fail "empty list in sumColor"
    | sumColor({r = x, g = y, b = z} :: nil) =
      {r = x, g = y, b = z}
    | sumColor({r = x, g = y, b = z} :: {r = x1, g = y1, b = z1} :: tail) =
      sumColor({r = x + x1, g = y + y1, b = z + z1} ::
        tail)

  fun meanColor(nil) = raise Fail "empty list in meanColor"
    | meanColor(list) =
      let
        val {r = x, g = y, b = z} = sumColor(list)
        val len = length(list)
      in
        {r = x div len, g = y div len, b = z div len}
      end


  fun findmin(nil) = raise Fail "empty list in findmin"
    | findmin([n]) = n
    | findmin(head::head2::tail) =
        if (head < head2) then
          findmin(head::tail)
        else
          findmin(head2::tail)

  fun remove(x, []) = raise Fail "empty list in remove"
    | remove(x, [n]) =
      if x = n then
        []
      else [n]
    | remove(x, head::tail) =
      if x = head then
        remove(x,tail)
      else
        head::remove(x, tail)

  fun selectionSort([]) = []
   | selectionSort([head]) = [head]
   | selectionSort(head1 :: head2 :: tail) =
      let
        val min = findmin(head1::head2::tail)
      in
        selectionSort([min] @ remove(min, (head1::head2::tail)))
      end


end
