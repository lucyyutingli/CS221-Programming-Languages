structure Read : sig

  val file : string -> string

end = struct

  fun file filename =
    let
      val instream = TextIO.openIn filename
      fun lp _ =
        (case TextIO.inputLine instream
	   of NONE => ""
	    | SOME line => line ^ lp ())
      val fileContents = lp ()
      val _ = TextIO.closeIn instream
    in
      fileContents
    end

end
	  
