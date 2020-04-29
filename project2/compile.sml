structure Compile : sig

  val steps : string -> InternalAST.term list
  val eval  : string -> Eval.normal_form

end = struct

(* how pretty/elegant is this? -ams ==> *)
  val steps = Eval.steps o Desugar.desugar o Parse.parse o Scan.scan
  val eval  = Eval.eval  o Desugar.desugar o Parse.parse o Scan.scan

end
