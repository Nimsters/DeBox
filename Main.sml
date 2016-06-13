structure Main =
struct

  fun createLexerStream ( is : BasicIO.instream ) =
      Lexing.createLexer (fn buff => fn n => Nonstdio.buff_input is buff 0 n)

  fun parse filename    =  
      let val lexbuf    = createLexerStream (BasicIO.open_in filename)
      in
            (Parser.Proof Lexer.Token lexbuf)
      end

  fun validate filename =  
      let val proof     = parse filename
          val (v, f)    = Validation.valid proof
          val message   = 
              if v 
              then "Your proof is valid, and a BoxProver script of it "
              else "Your proof is not valid, and a list of errors "
      in
        TextIO.output (TextIO.stdOut, message^"can be found in:\n"^f^".\n")
      end

  fun errorMess s = TextIO.output (TextIO.stdErr,s ^ "\n");

  val _ = case (Mosml.argv ()) of
            [_, "compare", file] => (
                    let val p1   = Validation.dummy
                        val p2   = parse file
                        val neg  = if (p1=p2) then "" else "not "
                        val msg  = "The two proofs are "^neg^
                                   "identical at the abstract level.\n"
                    in
                        TextIO.output(TextIO.stdOut, msg)
                    end
                    handle Lexer.LexicalError (mess,(lin,col)) =>
                           errorMess ("Lexical error: " ^mess^ " at line "
                            ^ makestring lin ^ ", column " ^ makestring col)
                         | Parsing.ParseError ob =>
                             let val Location.Loc (p1,p2)
                                       = Location.getCurrentLocation ()
                                 val (lin,col)
		                   = Lexer.getLineCol p2
		          	        	(!Lexer.currentLine)
		          	        	(!Lexer.lineStartPos)
                             in
                               errorMess ("Parse-error at line "
                             ^ makestring lin ^ ", column " ^ makestring col)
                             end
                    )
          | [_, "print"] => let val s = Validation.english Validation.dummy
                            in
                                TextIO.output(TextIO.stdOut, s^"\n")
                            end
          | [_, file]    => (validate (file)
                handle Parsing.yyexit ob => errorMess "Parser-exit\n"
                     | Parsing.ParseError ob =>
                         let val Location.Loc (p1,p2)
                                   = Location.getCurrentLocation ()
                             val (lin,col)
		               = Lexer.getLineCol p2
		          			(!Lexer.currentLine)
		          			(!Lexer.lineStartPos)
                         in
                           errorMess ("Parse-error at line "
                            ^ makestring lin ^ ", column " ^ makestring col)
                         end
                     | Lexer.LexicalError (mess,(lin,col)) =>
                           errorMess ("Lexical error: " ^mess^ " at line "
                            ^ makestring lin ^ ", column " ^ makestring col)
	                 | Validation.ValidationError (mess) =>
                           errorMess ("Validation error: " ^mess)
                     | SysErr (s,_) => errorMess ("Exception: " ^ s))
          | _ => TextIO.output(TextIO.stdErr, "Wrong number of arguments\n")

end
