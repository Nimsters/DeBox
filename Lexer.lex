{
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
 
 fun keyword s =
     case s of
        "to"        => Parser.TO ()
      | "From"      => Parser.FROM ()
      | "and"       => Parser.PLUS ()
      | "Assume"    => Parser.ASS ()
      | _           => Parser.ID s
}

rule Token = parse
        [` ` `\t` `\r`]         {Token lexbuf} (* Ignore whitespace *)
      | [`\n` `\012`]           {currentLine  := !currentLine+1;
                                 lineStartPos := getLexemeStart lexbuf::
                                !lineStartPos;
                                 Token lexbuf}
      | "We wish to prove"      {Parser.WWTP ()}
      | ", we wish to prove"    {Parser.WTP  ()}
      | "the premise"           {Parser.PREM ()}
      | "the premises"          {Parser.PRMS ()}
      | ", and"                 {Parser.PLUS ()}
      | ", we get"              {Parser.WG  ()}
      | ", we conclude"         {Parser.WC  ()}
      | "we get"                
      {lexerError lexbuf "You need a comma before the main clause"}
      | "we conclude"
      {lexerError lexbuf "You need a comma before the main clause"}
      | "We have the premise"                           {Parser.PRM ()}
      | "Discharge assumption"                          {Parser.DIS ()}
      | "By copying"                                    {Parser.CPY ()}
      | "By applying the and-introduction rule"         {Parser.AIN ()}
      | "By applying the first and-elimination rule"    {Parser.AE1 ()}
      | "By applying the second and-elimination rule"   {Parser.AE2 ()}
      | "By applying the first or-introduction rule"    {Parser.OI1 ()}
      | "By applying the second or-introduction rule"   {Parser.OI2 ()}
      | "By applying the or-elimination rule"           {Parser.OEL ()}
      | "By applying the implication-introduction rule" {Parser.IIN ()}
      | "By applying the implication-elimination rule"  {Parser.IEL ()}
      | "By applying the negation-introduction rule"    {Parser.NIN ()}
      | "By applying the negation-elimination rule"     {Parser.NEL ()}
      | "By applying introduction of double-negation"   {Parser.DIN ()}
      | "By applying elimination of double-negation"    {Parser.DEL ()}
      | "By applying elimination of absurdity"          {Parser.BEL ()}
      | "By applying Modus Tollens"                     {Parser.MOD ()}
      | "By applying proof by contradiction"            {Parser.PBC ()}
      | "By applying the law of the excluded middle"    {Parser.LEM ()}
      | ([`a`-`z`] | [`A`-`Z`] | [`0`-`9`])
        ([`a`-`z`] | [`A`-`Z`] | [`-` `_`] | [`0`-`9`])*
                                           {keyword (getLexeme lexbuf)}
      | "\226\138\165"                                  {Parser.BOT ()}
      | "_|_"                                           {Parser.BOT ()}
      | "\194\172"                                      {Parser.NEG ()}
      | "~"                                             {Parser.NEG ()}
      | "\226\136\167"                                  {Parser.AND ()}
      | "/\\"                                           {Parser.AND ()}
      | "\226\136\168"                                  {Parser.OR  ()}
      | "\\/"                                           {Parser.OR  ()}
      | "\226\134\146"                                  {Parser.IMP ()}
      | "->"                                            {Parser.IMP ()}
      | "=>"                                            {Parser.IMP ()}
      | `.`                                             {Parser.DOT ()}
      | eof                                             {Parser.EOF ()}
      | `(`                                             {Parser.LPAR ()}
      | `)`                                             {Parser.RPAR ()}
      | `[`                                             {Parser.LBRA ()}
      | `]`                                             {Parser.RBRA ()}
      | `-`                                             {Parser.DASH ()}
      | `:`                                             {Parser.COLON ()}
      | `,`                                             {Parser.COMMA ()}
      | _                 {lexerError lexbuf "Illegal phrasing in input"}
      ;
