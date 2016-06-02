
SRCS := Parser.sml Lexer.sml Proof.sml Validation.sml Unittest.sml
OBJS := $(SRCS:.sml=.ui) $(SRCS:.sml=.uo)

all : Main Test

Main : $(OBJS) Parser.grm
	mosmlc -o Main Main.sml

Test : Unittest.ui Auxiliaries.ui Proof.ui
	mosmlc -o Unittest Test.sml

Auxiliaries.uo Auxiliaries.ui : Auxiliaries.sml
	mosmlc -c $^

Unittest.sml : Auxiliaries.ui

Unittest.uo Unittest.ui : Unittest.sml
	mosmlc -c $^

Validation.sml : Unittest.ui

Validation.uo : Validation.sml 
	mosmlc -c $^

Validation.ui : Validation.sig
	mosmlc -c $^

Proof.uo Proof.ui : Proof.sml
	mosmlc -c $^

Lexer.uo Lexer.ui : Lexer.sml
	mosmlc -c $^

Lexer.sml : Lexer.lex Proof.sml Parser.grm
	mosmllex Lexer.lex

Parser.sml Parser.sig : Parser.grm Proof.sml
	mosmlyac -v Parser.grm

Parser.ui Parser.uo : Parser.sig Parser.sml Proof.ui
	mosmlc -c Parser.sig Parser.sml

clean:
	rm -f *.ui *.uo Parser.sig Parser.sml Lexer.sml Main Unittest

