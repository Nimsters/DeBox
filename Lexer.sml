local open Obj Lexing in


 open Lexing;

 val currentLine = ref 1
 val lineStartPos = ref [0]

 exception LexicalError of string * (int*int)

 fun getPos lexbuf = getLineCol (getLexemeStart lexbuf)
(!currentLine)
(!lineStartPos)

 and getLineCol pos line (p1::ps) =
        if pos >= p1 then (line, pos-p1)
        else getLineCol pos (line-1) ps
   | getLineCol pos line [] = 
     raise LexicalError ("Glitch in position retrieval",(0,0))

 fun lexerError lexbuf s =
        raise LexicalError (s, getPos lexbuf)
 

fun action_54 lexbuf = (
lexerError lexbuf "Illegal phrasing in input")
and action_53 lexbuf = (
Parser.COMMA ())
and action_52 lexbuf = (
Parser.COLON ())
and action_51 lexbuf = (
Parser.DASH ())
and action_50 lexbuf = (
Parser.LBRA ())
and action_49 lexbuf = (
Parser.LBRA ())
and action_48 lexbuf = (
Parser.RPAR ())
and action_47 lexbuf = (
Parser.LPAR ())
and action_46 lexbuf = (
Parser.EOF ())
and action_45 lexbuf = (
Parser.DOT ())
and action_44 lexbuf = (
Parser.IMP ())
and action_43 lexbuf = (
Parser.IMP ())
and action_42 lexbuf = (
Parser.IMP ())
and action_41 lexbuf = (
Parser.OR  ())
and action_40 lexbuf = (
Parser.OR  ())
and action_39 lexbuf = (
Parser.AND ())
and action_38 lexbuf = (
Parser.AND ())
and action_37 lexbuf = (
Parser.NEG ())
and action_36 lexbuf = (
Parser.NEG ())
and action_35 lexbuf = (
Parser.BOT ())
and action_34 lexbuf = (
Parser.BOT ())
and action_33 lexbuf = (
Parser.ID (getLexeme lexbuf))
and action_32 lexbuf = (
Parser.LEM ())
and action_31 lexbuf = (
Parser.PBC ())
and action_30 lexbuf = (
Parser.MOD ())
and action_29 lexbuf = (
Parser.BEL ())
and action_28 lexbuf = (
Parser.DEL ())
and action_27 lexbuf = (
Parser.DIN ())
and action_26 lexbuf = (
Parser.NEL ())
and action_25 lexbuf = (
Parser.NIN ())
and action_24 lexbuf = (
Parser.IEL ())
and action_23 lexbuf = (
Parser.IIN ())
and action_22 lexbuf = (
Parser.OEL ())
and action_21 lexbuf = (
Parser.OI2 ())
and action_20 lexbuf = (
Parser.OI1 ())
and action_19 lexbuf = (
Parser.AE2 ())
and action_18 lexbuf = (
Parser.AE1 ())
and action_17 lexbuf = (
Parser.AIN ())
and action_16 lexbuf = (
Parser.CPY ())
and action_15 lexbuf = (
Parser.DIS ())
and action_14 lexbuf = (
Parser.ASS ())
and action_13 lexbuf = (
Parser.PRM ())
and action_12 lexbuf = (
lexerError lexbuf "You need a comma before the main clause")
and action_11 lexbuf = (
lexerError lexbuf "You need a comma before the main clause")
and action_10 lexbuf = (
Parser.WC  ())
and action_9 lexbuf = (
Parser.WG  ())
and action_8 lexbuf = (
Parser.PLUS ())
and action_7 lexbuf = (
Parser.PLUS ())
and action_6 lexbuf = (
Parser.PRMS ())
and action_5 lexbuf = (
Parser.PREM ())
and action_4 lexbuf = (
Parser.WTP  ())
and action_3 lexbuf = (
Parser.WWTP ())
and action_2 lexbuf = (
Parser.FROM ())
and action_1 lexbuf = (
currentLine  := !currentLine+1;
                                 lineStartPos := getLexemeStart lexbuf::
                                !lineStartPos;
                                 Token lexbuf)
and action_0 lexbuf = (
Token lexbuf)
and state_0 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"G" andalso currChar <= #"V" then  state_15 lexbuf
 else if currChar >= #"b" andalso currChar <= #"s" then  state_15 lexbuf
 else case currChar of
    #"C" => state_15 lexbuf
 |  #"E" => state_15 lexbuf
 |  #"Z" => state_15 lexbuf
 |  #"Y" => state_15 lexbuf
 |  #"X" => state_15 lexbuf
 |  #"v" => state_15 lexbuf
 |  #"u" => state_15 lexbuf
 |  #"z" => state_15 lexbuf
 |  #"y" => state_15 lexbuf
 |  #"x" => state_15 lexbuf
 |  #"\t" => action_0 lexbuf
 |  #"\r" => action_0 lexbuf
 |  #" " => action_0 lexbuf
 |  #"\n" => action_1 lexbuf
 |  #"\f" => action_1 lexbuf
 |  #"\226" => state_27 lexbuf
 |  #"\194" => state_26 lexbuf
 |  #"~" => action_37 lexbuf
 |  #"w" => state_24 lexbuf
 |  #"t" => state_23 lexbuf
 |  #"a" => state_22 lexbuf
 |  #"_" => state_21 lexbuf
 |  #"]" => action_50 lexbuf
 |  #"[" => action_49 lexbuf
 |  #"W" => state_18 lexbuf
 |  #"F" => state_17 lexbuf
 |  #"D" => state_16 lexbuf
 |  #"B" => state_14 lexbuf
 |  #"A" => state_13 lexbuf
 |  #"=" => state_12 lexbuf
 |  #":" => action_52 lexbuf
 |  #"/" => state_10 lexbuf
 |  #"." => action_45 lexbuf
 |  #"-" => state_8 lexbuf
 |  #"," => state_7 lexbuf
 |  #")" => action_48 lexbuf
 |  #"(" => action_47 lexbuf
 |  #"\^@" => action_46 lexbuf
 |  _ => action_54 lexbuf
 end)
and state_7 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_53);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #" " => state_499 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_8 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_51);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #">" => action_43 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_10 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_54);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\\" => action_39 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_12 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_54);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #">" => action_44 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_13 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_33);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"A" andalso currChar <= #"Z" then  state_36 lexbuf
 else if currChar >= #"a" andalso currChar <= #"r" then  state_36 lexbuf
 else if currChar >= #"t" andalso currChar <= #"z" then  state_36 lexbuf
 else case currChar of
    #"-" => state_36 lexbuf
 |  #"_" => state_36 lexbuf
 |  #"s" => state_491 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_14 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_33);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"A" andalso currChar <= #"Z" then  state_36 lexbuf
 else if currChar >= #"a" andalso currChar <= #"x" then  state_36 lexbuf
 else case currChar of
    #"-" => state_36 lexbuf
 |  #"_" => state_36 lexbuf
 |  #"z" => state_36 lexbuf
 |  #"y" => state_118 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_15 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_33);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"A" andalso currChar <= #"Z" then  state_36 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_36 lexbuf
 else case currChar of
    #"-" => state_36 lexbuf
 |  #"_" => state_36 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_16 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_33);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"A" andalso currChar <= #"Z" then  state_36 lexbuf
 else if currChar >= #"a" andalso currChar <= #"h" then  state_36 lexbuf
 else if currChar >= #"j" andalso currChar <= #"z" then  state_36 lexbuf
 else case currChar of
    #"-" => state_36 lexbuf
 |  #"_" => state_36 lexbuf
 |  #"i" => state_99 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_17 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_33);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"A" andalso currChar <= #"Z" then  state_36 lexbuf
 else if currChar >= #"a" andalso currChar <= #"q" then  state_36 lexbuf
 else if currChar >= #"s" andalso currChar <= #"z" then  state_36 lexbuf
 else case currChar of
    #"-" => state_36 lexbuf
 |  #"_" => state_36 lexbuf
 |  #"r" => state_96 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_18 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_33);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"A" andalso currChar <= #"Z" then  state_36 lexbuf
 else if currChar >= #"f" andalso currChar <= #"z" then  state_36 lexbuf
 else case currChar of
    #"-" => state_36 lexbuf
 |  #"_" => state_36 lexbuf
 |  #"d" => state_36 lexbuf
 |  #"c" => state_36 lexbuf
 |  #"b" => state_36 lexbuf
 |  #"a" => state_36 lexbuf
 |  #"e" => state_65 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_21 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_54);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"|" => state_63 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_22 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_33);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"A" andalso currChar <= #"Z" then  state_36 lexbuf
 else if currChar >= #"a" andalso currChar <= #"m" then  state_36 lexbuf
 else if currChar >= #"o" andalso currChar <= #"z" then  state_36 lexbuf
 else case currChar of
    #"-" => state_36 lexbuf
 |  #"_" => state_36 lexbuf
 |  #"n" => state_61 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_23 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_33);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"A" andalso currChar <= #"Z" then  state_36 lexbuf
 else if currChar >= #"a" andalso currChar <= #"g" then  state_36 lexbuf
 else if currChar >= #"i" andalso currChar <= #"z" then  state_36 lexbuf
 else case currChar of
    #"-" => state_36 lexbuf
 |  #"_" => state_36 lexbuf
 |  #"h" => state_50 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_24 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_33);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"A" andalso currChar <= #"Z" then  state_36 lexbuf
 else if currChar >= #"f" andalso currChar <= #"z" then  state_36 lexbuf
 else case currChar of
    #"-" => state_36 lexbuf
 |  #"_" => state_36 lexbuf
 |  #"d" => state_36 lexbuf
 |  #"c" => state_36 lexbuf
 |  #"b" => state_36 lexbuf
 |  #"a" => state_36 lexbuf
 |  #"e" => state_37 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_26 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_54);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\172" => action_36 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_27 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_54);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\138" => state_30 lexbuf
 |  #"\136" => state_29 lexbuf
 |  #"\134" => state_28 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_28 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\146" => action_42 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_29 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\168" => action_40 lexbuf
 |  #"\167" => action_38 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_30 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\165" => action_34 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_36 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_33);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"A" andalso currChar <= #"Z" then  state_36 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_36 lexbuf
 else case currChar of
    #"-" => state_36 lexbuf
 |  #"_" => state_36 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_37 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_33);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"A" andalso currChar <= #"Z" then  state_36 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_36 lexbuf
 else case currChar of
    #"-" => state_36 lexbuf
 |  #"_" => state_36 lexbuf
 |  #" " => state_38 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_38 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"g" => state_40 lexbuf
 |  #"c" => state_39 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_39 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"o" => state_43 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_40 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"e" => state_41 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_41 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"t" => action_11 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_43 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"n" => state_44 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_44 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"c" => state_45 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_45 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"l" => state_46 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_46 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"u" => state_47 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_47 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"d" => state_48 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_48 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"e" => action_12 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_50 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_33);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"A" andalso currChar <= #"Z" then  state_36 lexbuf
 else if currChar >= #"f" andalso currChar <= #"z" then  state_36 lexbuf
 else case currChar of
    #"-" => state_36 lexbuf
 |  #"_" => state_36 lexbuf
 |  #"d" => state_36 lexbuf
 |  #"c" => state_36 lexbuf
 |  #"b" => state_36 lexbuf
 |  #"a" => state_36 lexbuf
 |  #"e" => state_51 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_51 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_33);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"A" andalso currChar <= #"Z" then  state_36 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_36 lexbuf
 else case currChar of
    #"-" => state_36 lexbuf
 |  #"_" => state_36 lexbuf
 |  #" " => state_52 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_52 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"p" => state_53 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_53 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"r" => state_54 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_54 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"e" => state_55 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_55 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"m" => state_56 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_56 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"i" => state_57 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_57 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"s" => state_58 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_58 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"e" => state_59 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_59 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_5);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"s" => action_6 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_61 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_33);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"A" andalso currChar <= #"Z" then  state_36 lexbuf
 else if currChar >= #"e" andalso currChar <= #"z" then  state_36 lexbuf
 else case currChar of
    #"-" => state_36 lexbuf
 |  #"_" => state_36 lexbuf
 |  #"c" => state_36 lexbuf
 |  #"b" => state_36 lexbuf
 |  #"a" => state_36 lexbuf
 |  #"d" => state_62 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_62 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_8);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"A" andalso currChar <= #"Z" then  state_36 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_36 lexbuf
 else case currChar of
    #"-" => state_36 lexbuf
 |  #"_" => state_36 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_63 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"_" => action_35 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_65 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_33);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"A" andalso currChar <= #"Z" then  state_36 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_36 lexbuf
 else case currChar of
    #"-" => state_36 lexbuf
 |  #"_" => state_36 lexbuf
 |  #" " => state_66 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_66 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"w" => state_68 lexbuf
 |  #"h" => state_67 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_67 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"a" => state_81 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_68 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"i" => state_69 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_69 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"s" => state_70 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_70 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"h" => state_71 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_71 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #" " => state_72 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_72 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"t" => state_73 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_73 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"o" => state_74 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_74 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #" " => state_75 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_75 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"p" => state_76 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_76 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"r" => state_77 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_77 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"o" => state_78 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_78 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"v" => state_79 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_79 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"e" => action_3 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_81 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"v" => state_82 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_82 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"e" => state_83 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_83 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #" " => state_84 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_84 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"t" => state_85 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_85 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"h" => state_86 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_86 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"e" => state_87 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_87 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #" " => state_88 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_88 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"p" => state_89 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_89 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"r" => state_90 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_90 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"e" => state_91 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_91 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"m" => state_92 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_92 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"i" => state_93 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_93 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"s" => state_94 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_94 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"e" => action_13 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_96 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_33);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"A" andalso currChar <= #"Z" then  state_36 lexbuf
 else if currChar >= #"a" andalso currChar <= #"n" then  state_36 lexbuf
 else if currChar >= #"p" andalso currChar <= #"z" then  state_36 lexbuf
 else case currChar of
    #"-" => state_36 lexbuf
 |  #"_" => state_36 lexbuf
 |  #"o" => state_97 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_97 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_33);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"A" andalso currChar <= #"Z" then  state_36 lexbuf
 else if currChar >= #"a" andalso currChar <= #"l" then  state_36 lexbuf
 else if currChar >= #"n" andalso currChar <= #"z" then  state_36 lexbuf
 else case currChar of
    #"-" => state_36 lexbuf
 |  #"_" => state_36 lexbuf
 |  #"m" => state_98 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_98 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_2);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"A" andalso currChar <= #"Z" then  state_36 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_36 lexbuf
 else case currChar of
    #"-" => state_36 lexbuf
 |  #"_" => state_36 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_99 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_33);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"A" andalso currChar <= #"Z" then  state_36 lexbuf
 else if currChar >= #"a" andalso currChar <= #"r" then  state_36 lexbuf
 else if currChar >= #"t" andalso currChar <= #"z" then  state_36 lexbuf
 else case currChar of
    #"-" => state_36 lexbuf
 |  #"_" => state_36 lexbuf
 |  #"s" => state_100 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_100 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_33);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"A" andalso currChar <= #"Z" then  state_36 lexbuf
 else if currChar >= #"d" andalso currChar <= #"z" then  state_36 lexbuf
 else case currChar of
    #"-" => state_36 lexbuf
 |  #"_" => state_36 lexbuf
 |  #"b" => state_36 lexbuf
 |  #"a" => state_36 lexbuf
 |  #"c" => state_101 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_101 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_33);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"A" andalso currChar <= #"Z" then  state_36 lexbuf
 else if currChar >= #"a" andalso currChar <= #"g" then  state_36 lexbuf
 else if currChar >= #"i" andalso currChar <= #"z" then  state_36 lexbuf
 else case currChar of
    #"-" => state_36 lexbuf
 |  #"_" => state_36 lexbuf
 |  #"h" => state_102 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_102 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_33);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"A" andalso currChar <= #"Z" then  state_36 lexbuf
 else if currChar >= #"b" andalso currChar <= #"z" then  state_36 lexbuf
 else case currChar of
    #"-" => state_36 lexbuf
 |  #"_" => state_36 lexbuf
 |  #"a" => state_103 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_103 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_33);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"A" andalso currChar <= #"Z" then  state_36 lexbuf
 else if currChar >= #"a" andalso currChar <= #"q" then  state_36 lexbuf
 else if currChar >= #"s" andalso currChar <= #"z" then  state_36 lexbuf
 else case currChar of
    #"-" => state_36 lexbuf
 |  #"_" => state_36 lexbuf
 |  #"r" => state_104 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_104 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_33);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"A" andalso currChar <= #"Z" then  state_36 lexbuf
 else if currChar >= #"a" andalso currChar <= #"f" then  state_36 lexbuf
 else if currChar >= #"h" andalso currChar <= #"z" then  state_36 lexbuf
 else case currChar of
    #"-" => state_36 lexbuf
 |  #"_" => state_36 lexbuf
 |  #"g" => state_105 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_105 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_33);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"A" andalso currChar <= #"Z" then  state_36 lexbuf
 else if currChar >= #"f" andalso currChar <= #"z" then  state_36 lexbuf
 else case currChar of
    #"-" => state_36 lexbuf
 |  #"_" => state_36 lexbuf
 |  #"d" => state_36 lexbuf
 |  #"c" => state_36 lexbuf
 |  #"b" => state_36 lexbuf
 |  #"a" => state_36 lexbuf
 |  #"e" => state_106 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_106 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_33);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"A" andalso currChar <= #"Z" then  state_36 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_36 lexbuf
 else case currChar of
    #"-" => state_36 lexbuf
 |  #"_" => state_36 lexbuf
 |  #" " => state_107 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_107 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"a" => state_108 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_108 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"s" => state_109 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_109 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"s" => state_110 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_110 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"u" => state_111 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_111 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"m" => state_112 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_112 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"p" => state_113 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_113 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"t" => state_114 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_114 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"i" => state_115 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_115 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"o" => state_116 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_116 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"n" => action_15 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_118 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_33);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"A" andalso currChar <= #"Z" then  state_36 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_36 lexbuf
 else case currChar of
    #"-" => state_36 lexbuf
 |  #"_" => state_36 lexbuf
 |  #" " => state_119 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_119 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"c" => state_121 lexbuf
 |  #"a" => state_120 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_120 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"p" => state_128 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_121 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"o" => state_122 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_122 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"p" => state_123 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_123 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"y" => state_124 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_124 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"i" => state_125 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_125 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"n" => state_126 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_126 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"g" => action_16 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_128 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"p" => state_129 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_129 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"l" => state_130 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_130 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"y" => state_131 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_131 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"i" => state_132 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_132 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"n" => state_133 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_133 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"g" => state_134 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_134 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #" " => state_135 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_135 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"t" => state_140 lexbuf
 |  #"p" => state_139 lexbuf
 |  #"i" => state_138 lexbuf
 |  #"e" => state_137 lexbuf
 |  #"M" => state_136 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_136 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"o" => state_479 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_137 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"l" => state_441 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_138 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"n" => state_411 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_139 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"r" => state_390 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_140 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"h" => state_141 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_141 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"e" => state_142 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_142 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #" " => state_143 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_143 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"s" => state_150 lexbuf
 |  #"o" => state_149 lexbuf
 |  #"n" => state_148 lexbuf
 |  #"l" => state_147 lexbuf
 |  #"i" => state_146 lexbuf
 |  #"f" => state_145 lexbuf
 |  #"a" => state_144 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_144 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"n" => state_370 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_145 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"i" => state_325 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_146 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"m" => state_281 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_147 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"a" => state_256 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_148 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"e" => state_215 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_149 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"r" => state_197 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_150 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"e" => state_151 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_151 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"c" => state_152 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_152 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"o" => state_153 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_153 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"n" => state_154 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_154 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"d" => state_155 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_155 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #" " => state_156 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_156 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"o" => state_158 lexbuf
 |  #"a" => state_157 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_157 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"n" => state_178 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_158 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"r" => state_159 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_159 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"-" => state_160 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_160 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"i" => state_161 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_161 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"n" => state_162 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_162 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"t" => state_163 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_163 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"r" => state_164 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_164 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"o" => state_165 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_165 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"d" => state_166 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_166 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"u" => state_167 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_167 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"c" => state_168 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_168 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"t" => state_169 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_169 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"i" => state_170 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_170 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"o" => state_171 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_171 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"n" => state_172 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_172 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #" " => state_173 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_173 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"r" => state_174 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_174 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"u" => state_175 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_175 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"l" => state_176 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_176 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"e" => action_21 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_178 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"d" => state_179 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_179 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"-" => state_180 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_180 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"e" => state_181 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_181 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"l" => state_182 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_182 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"i" => state_183 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_183 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"m" => state_184 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_184 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"i" => state_185 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_185 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"n" => state_186 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_186 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"a" => state_187 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_187 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"t" => state_188 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_188 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"i" => state_189 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_189 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"o" => state_190 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_190 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"n" => state_191 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_191 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #" " => state_192 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_192 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"r" => state_193 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_193 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"u" => state_194 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_194 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"l" => state_195 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_195 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"e" => action_19 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_197 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"-" => state_198 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_198 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"e" => state_199 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_199 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"l" => state_200 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_200 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"i" => state_201 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_201 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"m" => state_202 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_202 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"i" => state_203 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_203 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"n" => state_204 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_204 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"a" => state_205 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_205 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"t" => state_206 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_206 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"i" => state_207 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_207 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"o" => state_208 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_208 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"n" => state_209 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_209 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #" " => state_210 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_210 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"r" => state_211 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_211 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"u" => state_212 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_212 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"l" => state_213 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_213 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"e" => action_22 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_215 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"g" => state_216 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_216 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"a" => state_217 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_217 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"t" => state_218 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_218 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"i" => state_219 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_219 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"o" => state_220 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_220 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"n" => state_221 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_221 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"-" => state_222 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_222 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"i" => state_224 lexbuf
 |  #"e" => state_223 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_223 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"l" => state_241 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_224 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"n" => state_225 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_225 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"t" => state_226 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_226 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"r" => state_227 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_227 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"o" => state_228 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_228 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"d" => state_229 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_229 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"u" => state_230 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_230 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"c" => state_231 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_231 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"t" => state_232 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_232 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"i" => state_233 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_233 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"o" => state_234 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_234 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"n" => state_235 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_235 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #" " => state_236 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_236 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"r" => state_237 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_237 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"u" => state_238 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_238 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"l" => state_239 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_239 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"e" => action_25 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_241 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"i" => state_242 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_242 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"m" => state_243 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_243 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"i" => state_244 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_244 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"n" => state_245 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_245 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"a" => state_246 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_246 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"t" => state_247 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_247 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"i" => state_248 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_248 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"o" => state_249 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_249 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"n" => state_250 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_250 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #" " => state_251 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_251 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"r" => state_252 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_252 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"u" => state_253 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_253 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"l" => state_254 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_254 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"e" => action_26 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_256 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"w" => state_257 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_257 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #" " => state_258 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_258 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"o" => state_259 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_259 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"f" => state_260 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_260 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #" " => state_261 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_261 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"t" => state_262 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_262 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"h" => state_263 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_263 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"e" => state_264 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_264 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #" " => state_265 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_265 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"e" => state_266 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_266 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"x" => state_267 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_267 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"c" => state_268 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_268 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"l" => state_269 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_269 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"u" => state_270 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_270 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"d" => state_271 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_271 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"e" => state_272 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_272 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"d" => state_273 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_273 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #" " => state_274 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_274 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"m" => state_275 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_275 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"i" => state_276 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_276 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"d" => state_277 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_277 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"d" => state_278 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_278 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"l" => state_279 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_279 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"e" => action_32 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_281 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"p" => state_282 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_282 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"l" => state_283 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_283 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"i" => state_284 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_284 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"c" => state_285 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_285 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"a" => state_286 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_286 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"t" => state_287 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_287 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"i" => state_288 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_288 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"o" => state_289 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_289 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"n" => state_290 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_290 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"-" => state_291 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_291 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"i" => state_293 lexbuf
 |  #"e" => state_292 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_292 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"l" => state_310 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_293 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"n" => state_294 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_294 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"t" => state_295 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_295 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"r" => state_296 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_296 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"o" => state_297 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_297 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"d" => state_298 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_298 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"u" => state_299 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_299 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"c" => state_300 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_300 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"t" => state_301 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_301 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"i" => state_302 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_302 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"o" => state_303 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_303 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"n" => state_304 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_304 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #" " => state_305 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_305 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"r" => state_306 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_306 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"u" => state_307 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_307 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"l" => state_308 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_308 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"e" => action_23 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_310 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"i" => state_311 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_311 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"m" => state_312 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_312 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"i" => state_313 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_313 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"n" => state_314 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_314 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"a" => state_315 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_315 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"t" => state_316 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_316 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"i" => state_317 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_317 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"o" => state_318 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_318 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"n" => state_319 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_319 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #" " => state_320 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_320 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"r" => state_321 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_321 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"u" => state_322 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_322 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"l" => state_323 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_323 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"e" => action_24 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_325 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"r" => state_326 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_326 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"s" => state_327 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_327 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"t" => state_328 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_328 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #" " => state_329 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_329 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"o" => state_331 lexbuf
 |  #"a" => state_330 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_330 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"n" => state_351 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_331 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"r" => state_332 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_332 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"-" => state_333 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_333 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"i" => state_334 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_334 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"n" => state_335 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_335 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"t" => state_336 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_336 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"r" => state_337 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_337 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"o" => state_338 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_338 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"d" => state_339 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_339 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"u" => state_340 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_340 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"c" => state_341 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_341 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"t" => state_342 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_342 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"i" => state_343 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_343 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"o" => state_344 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_344 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"n" => state_345 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_345 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #" " => state_346 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_346 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"r" => state_347 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_347 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"u" => state_348 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_348 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"l" => state_349 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_349 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"e" => action_20 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_351 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"d" => state_352 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_352 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"-" => state_353 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_353 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"e" => state_354 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_354 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"l" => state_355 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_355 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"i" => state_356 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_356 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"m" => state_357 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_357 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"i" => state_358 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_358 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"n" => state_359 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_359 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"a" => state_360 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_360 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"t" => state_361 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_361 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"i" => state_362 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_362 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"o" => state_363 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_363 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"n" => state_364 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_364 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #" " => state_365 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_365 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"r" => state_366 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_366 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"u" => state_367 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_367 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"l" => state_368 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_368 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"e" => action_18 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_370 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"d" => state_371 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_371 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"-" => state_372 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_372 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"i" => state_373 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_373 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"n" => state_374 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_374 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"t" => state_375 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_375 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"r" => state_376 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_376 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"o" => state_377 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_377 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"d" => state_378 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_378 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"u" => state_379 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_379 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"c" => state_380 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_380 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"t" => state_381 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_381 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"i" => state_382 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_382 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"o" => state_383 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_383 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"n" => state_384 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_384 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #" " => state_385 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_385 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"r" => state_386 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_386 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"u" => state_387 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_387 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"l" => state_388 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_388 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"e" => action_17 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_390 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"o" => state_391 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_391 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"o" => state_392 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_392 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"f" => state_393 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_393 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #" " => state_394 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_394 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"b" => state_395 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_395 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"y" => state_396 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_396 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #" " => state_397 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_397 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"c" => state_398 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_398 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"o" => state_399 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_399 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"n" => state_400 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_400 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"t" => state_401 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_401 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"r" => state_402 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_402 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"a" => state_403 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_403 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"d" => state_404 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_404 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"i" => state_405 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_405 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"c" => state_406 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_406 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"t" => state_407 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_407 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"i" => state_408 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_408 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"o" => state_409 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_409 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"n" => action_31 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_411 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"t" => state_412 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_412 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"r" => state_413 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_413 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"o" => state_414 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_414 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"d" => state_415 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_415 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"u" => state_416 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_416 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"c" => state_417 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_417 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"t" => state_418 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_418 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"i" => state_419 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_419 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"o" => state_420 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_420 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"n" => state_421 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_421 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #" " => state_422 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_422 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"o" => state_423 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_423 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"f" => state_424 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_424 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #" " => state_425 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_425 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"d" => state_426 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_426 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"o" => state_427 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_427 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"u" => state_428 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_428 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"b" => state_429 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_429 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"l" => state_430 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_430 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"e" => state_431 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_431 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"-" => state_432 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_432 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"n" => state_433 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_433 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"e" => state_434 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_434 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"g" => state_435 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_435 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"a" => state_436 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_436 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"t" => state_437 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_437 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"i" => state_438 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_438 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"o" => state_439 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_439 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"n" => action_27 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_441 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"i" => state_442 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_442 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"m" => state_443 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_443 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"i" => state_444 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_444 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"n" => state_445 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_445 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"a" => state_446 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_446 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"t" => state_447 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_447 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"i" => state_448 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_448 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"o" => state_449 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_449 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"n" => state_450 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_450 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #" " => state_451 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_451 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"o" => state_452 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_452 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"f" => state_453 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_453 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #" " => state_454 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_454 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"d" => state_456 lexbuf
 |  #"a" => state_455 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_455 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"b" => state_471 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_456 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"o" => state_457 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_457 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"u" => state_458 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_458 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"b" => state_459 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_459 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"l" => state_460 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_460 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"e" => state_461 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_461 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"-" => state_462 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_462 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"n" => state_463 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_463 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"e" => state_464 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_464 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"g" => state_465 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_465 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"a" => state_466 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_466 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"t" => state_467 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_467 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"i" => state_468 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_468 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"o" => state_469 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_469 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"n" => action_28 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_471 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"s" => state_472 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_472 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"u" => state_473 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_473 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"r" => state_474 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_474 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"d" => state_475 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_475 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"i" => state_476 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_476 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"t" => state_477 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_477 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"y" => action_29 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_479 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"d" => state_480 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_480 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"u" => state_481 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_481 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"s" => state_482 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_482 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #" " => state_483 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_483 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"T" => state_484 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_484 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"o" => state_485 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_485 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"l" => state_486 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_486 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"l" => state_487 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_487 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"e" => state_488 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_488 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"n" => state_489 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_489 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"s" => action_30 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_491 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_33);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"A" andalso currChar <= #"Z" then  state_36 lexbuf
 else if currChar >= #"a" andalso currChar <= #"r" then  state_36 lexbuf
 else if currChar >= #"t" andalso currChar <= #"z" then  state_36 lexbuf
 else case currChar of
    #"-" => state_36 lexbuf
 |  #"_" => state_36 lexbuf
 |  #"s" => state_492 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_492 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_33);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"A" andalso currChar <= #"Z" then  state_36 lexbuf
 else if currChar >= #"a" andalso currChar <= #"t" then  state_36 lexbuf
 else case currChar of
    #"-" => state_36 lexbuf
 |  #"_" => state_36 lexbuf
 |  #"z" => state_36 lexbuf
 |  #"y" => state_36 lexbuf
 |  #"x" => state_36 lexbuf
 |  #"w" => state_36 lexbuf
 |  #"v" => state_36 lexbuf
 |  #"u" => state_493 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_493 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_33);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"A" andalso currChar <= #"Z" then  state_36 lexbuf
 else if currChar >= #"a" andalso currChar <= #"l" then  state_36 lexbuf
 else if currChar >= #"n" andalso currChar <= #"z" then  state_36 lexbuf
 else case currChar of
    #"-" => state_36 lexbuf
 |  #"_" => state_36 lexbuf
 |  #"m" => state_494 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_494 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_33);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"A" andalso currChar <= #"Z" then  state_36 lexbuf
 else if currChar >= #"f" andalso currChar <= #"z" then  state_36 lexbuf
 else case currChar of
    #"-" => state_36 lexbuf
 |  #"_" => state_36 lexbuf
 |  #"d" => state_36 lexbuf
 |  #"c" => state_36 lexbuf
 |  #"b" => state_36 lexbuf
 |  #"a" => state_36 lexbuf
 |  #"e" => state_495 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_495 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_14);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"A" andalso currChar <= #"Z" then  state_36 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_36 lexbuf
 else case currChar of
    #"-" => state_36 lexbuf
 |  #"_" => state_36 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_499 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"w" => state_501 lexbuf
 |  #"a" => state_500 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_500 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"n" => state_528 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_501 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"e" => state_502 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_502 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #" " => state_503 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_503 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"w" => state_506 lexbuf
 |  #"g" => state_505 lexbuf
 |  #"c" => state_504 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_504 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"o" => state_521 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_505 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"e" => state_519 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_506 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"i" => state_507 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_507 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"s" => state_508 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_508 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"h" => state_509 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_509 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #" " => state_510 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_510 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"t" => state_511 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_511 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"o" => state_512 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_512 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #" " => state_513 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_513 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"p" => state_514 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_514 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"r" => state_515 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_515 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"o" => state_516 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_516 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"v" => state_517 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_517 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"e" => action_4 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_519 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"t" => action_9 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_521 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"n" => state_522 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_522 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"c" => state_523 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_523 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"l" => state_524 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_524 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"u" => state_525 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_525 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"d" => state_526 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_526 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"e" => action_10 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_528 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"d" => action_7 lexbuf
 |  _ => backtrack lexbuf
 end)
and Token lexbuf =
  (setLexLastAction lexbuf (magic dummyAction);
   setLexStartPos lexbuf (getLexCurrPos lexbuf);
   state_0 lexbuf)

(* The following checks type consistency of actions *)
val _ = fn _ => [action_54, action_53, action_52, action_51, action_50, action_49, action_48, action_47, action_46, action_45, action_44, action_43, action_42, action_41, action_40, action_39, action_38, action_37, action_36, action_35, action_34, action_33, action_32, action_31, action_30, action_29, action_28, action_27, action_26, action_25, action_24, action_23, action_22, action_21, action_20, action_19, action_18, action_17, action_16, action_15, action_14, action_13, action_12, action_11, action_10, action_9, action_8, action_7, action_6, action_5, action_4, action_3, action_2, action_1, action_0];

end
