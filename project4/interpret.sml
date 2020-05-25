structure Interpret : sig

  val interpret      : (ULC.term -> ULC.term option) -> string -> ULC.term
  val interpretSteps : (ULC.term -> ULC.term option) -> string -> ULC.term list

  val cbv          : string -> ULC.term
  val fullBeta     : string -> ULC.term	
  val normalOrder  : string -> ULC.term	
  val lazy         : string -> ULC.term

  val cbv'         : string -> ULC.term list
  val fullBeta'    : string -> ULC.term	list
  val normalOrder' : string -> ULC.term	list
  val lazy'        : string -> ULC.term list

  val file  : (ULC.term -> ULC.term option) -> string -> ULC.term
  val file' : (ULC.term -> ULC.term option) -> string -> unit

end = struct

  fun interp eval step code =
    let
      val tokens = Scan.scan code
      val sast   = Parse.parse tokens
      val sast'  = Inline.inline sast
      val ast    = Desugar.desugar sast'
    in
      eval step ast
    end

  fun interpret step code      = interp Eval.eval step code
  fun interpretSteps step code = interp Eval.evalSteps step code

  val cbv          = interpret CBV.step
  val fullBeta     = interpret FullBeta.step
  val normalOrder  = interpret NormalOrder.step
  val lazy         = interpret Lazy.step

  val cbv'         = interpretSteps CBV.step
  val fullBeta'    = interpretSteps FullBeta.step
  val normalOrder' = interpretSteps NormalOrder.step
  val lazy'        = interpretSteps Lazy.step

  fun file step filename = interpret step (Read.file filename)

  fun file' step filename =
    let
      fun toksStr ts = "[" ^ String.concatWith "," (List.map Token.tos ts) ^ "]"
      fun println s = (TextIO.print s; TextIO.print "\n")
      fun announce phase = println ("\n>>>>>>>>>>>>>>> " ^ phase ^ "...")
      val code = Read.file filename
      val _ = println (filename ^ ":\n" ^ code)
      val _ = announce "scanning"
      val tokens = Scan.scan code
      val _ = println (toksStr tokens)
      val _ = announce "parsing"
      val sast = Parse.parse tokens
      val _ = println (SULC.prog sast)
      val _ = announce "inlining"
      val sast' = Inline.inline sast
      val _ = println (SULC.tm sast')
      val _ = announce "desugaring"
      val ast = Desugar.desugar sast'
      val _ = println (ULC.tos ast)
      val _ = announce "evaluating"
      val steps = Eval.evalSteps step ast
    in
      List.app (println o ULC.tos) steps
    end

end
