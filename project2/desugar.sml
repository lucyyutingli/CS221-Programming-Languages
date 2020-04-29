structure Desugar : sig

  val desugar : AST.term -> InternalAST.term

end = struct

  structure A = AST
  structure I = InternalAST

  fun desugar _ = raise Fail "todo: Desugar.desugar"

end
