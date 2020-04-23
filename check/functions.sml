structure Functions = struct

  fun fact 0 = 1
    | fact n = n * fact (n-1)

  fun exclaim s = s^"!"

  datatype fruit = Mango | Lime | Banana

  fun isSweet Lime = false
    | isSweet _ = true

end
