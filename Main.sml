structure Main =
struct

  fun createLexerStream ( is : BasicIO.instream ) =
      Lexing.createLexer (fn buff => fn n => Nonstdio.buff_input is buff 0 n)

  fun validate filename =  
      let val lexbuf    = createLexerStream (BasicIO.open_in filename)
          val proof     = (Parser.Proof Lexer.Token lexbuf)
          val (v, f)    = Validation.valid proof
          val message   = 
              if v 
              then "Your proof is valid, and a BoxProver script of it"
              else "Your proof is not valid, and a list of errors "
      in
        TextIO.output (TextIO.stdOut, message^"can be found in:\n"^f^".\n")
      end


  fun errorMess s = TextIO.output (TextIO.stdErr,s ^ "\n");

  val _ = validate (List.nth(Mosml.argv (),1))
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
               | SysErr (s,_) => errorMess ("Exception: " ^ s)
end
