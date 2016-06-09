
SRCS := Parser.sml Lexer.sml Proof.sml Validation.sml Unittest.sml
OBJS := $(SRCS:.sml=.ui) $(SRCS:.sml=.uo)

all : Main Test

Main : $(OBJS) Parser.grm
	mosmlc -o Main Main.sml

Test : Unittest.ui Auxiliaries.ui Proof.ui
	mosmlc -o Unittest Test.sml

Auxiliaries.uo Auxiliaries.ui : Auxiliaries.sml Proof.sml
	mosmlc -c Auxiliaries.sml

Unittest.sml : Auxiliaries.ui

Unittest.uo Unittest.ui : Unittest.sml Auxiliaries.sml Proof.sml
	mosmlc -c Unittest.sml

Validation.sml : Unittest.ui Validation.ui Unittest.sml Auxiliaries.sml Proof.sml

Validation.uo : Validation.sml Unittest.sml Auxiliaries.sml Proof.sml
	mosmlc -c Validation.sml

Validation.ui : Validation.sig Unittest.sml Auxiliaries.sml Proof.sml
	mosmlc -c Validation.sig

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

