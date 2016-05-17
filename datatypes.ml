datatype formula    = Atom of char |
                      NEG  of formula |
                      AND  of formula*formula |
                      OR   of formula*formula |
                      IMP  of formula*formula |
                      BOT;

datatype rule       = Ass | Dis |       (* assumption and discharge *)
                      Prm | Cpy |       (* premise and copy *)
                      Ain | Ae1 | Ae2 | (* AND introduction/elimination *)
                      Oi1 | Oi2 | Oel | (* OR introduction/elimination *)
                      Iin | Iel |       (* IMP introduction/elimination *)
                      Nin | Nel |       (* NEG introduction/elimination *)
                      Din | Del |       (* double-negative (D) in/el *)
                      Bel |             (* absurdity (Bottom) elimination *)
                      Mod |             (* Modus Tollens *)
                      Pbc |             (* Proof by Contradiction *)
                      Lem               (* Law of the excluded middle *);

datatype reference  = Line of string | Box of string*string;

datatype proofstep  = Step of formula option*rule*reference list * string;

datatype sequent    = Sequent of formula list * formula;

datatype proof      = Proof of string * sequent * proofstep list;
