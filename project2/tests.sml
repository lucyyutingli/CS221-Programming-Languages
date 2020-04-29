structure Tests = struct

  structure I = InternalAST
  structure E = Eval

  val expect = Check.expect
  val eval = Compile.eval

  fun run () =
    let
      val _ = expect (eval "(if #t 1 0)", E.Value (I.Succ I.Zero), "compile1")
    in
      "tests done"
    end
 
end
