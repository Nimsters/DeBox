datatype formula = Atom of char |
                   Neg of formula |
                   Land of formula*formula |
                   Lor of formula*formula |
                   Imp of formula*formula;

fun bracket(s) = "("^s^")";

fun toString(Atom c)        = Char.toString c
  | toString(Neg f)         = "¬"^negToString(f)
  | toString(Land (f1,f2))  = andToString(f1)^"/\\"^andToString(f2)
  | toString(Lor (f1, f2))  = impToString(f1)^"\\/"^impToString(f2)
  | toString(Imp (f1, f2))  = impToString(f1)^"=>"^toString(f2)

and negToString(a as Atom _)    = toString(a)
  | negToString(n as Neg _)     = toString(n)
  | negToString(f as _)         = bracket(toString(f))

and andToString(f as Lor _)     = bracket(toString(f))
  | andToString(f as Imp _)     = bracket(toString(f))
  | andToString(f as _)         = toString(f)

and impToString(f as Imp _)      = bracket(toString(f))
  | impToString(f as _)          = toString(f);

fun printl s = print(s^"\n");

fun toNL(Atom c)        = Char.toString c
  | toNL(Neg f)         = negToNL(f)
  | toNL(f as Land _)   = andToNL(f)
  | toNL(f as Lor _)     = orToNL(f, " ")
  | toNL(Imp (f1, f2))  = impToNL(f1)^" implies that "^toNL(f2)

and negToNL(a as Atom _) = "not-"^toNL(a)
  | negToNL(f as Land _) = "not all of "^andToNL(f)
  | negToNL(f as Lor _)  = "neither "^orToNL(f, " n")
  | negToNL(f as Neg _)       = 
        let val n      = "not-"
            val result = toNL(f)
        in
            if (String.isPrefix n result) 
            then n^result
            else "it is not the case that "^result
        end
  | negToNL(f as Imp _)  = "the implication \""^impToNL(f)^"\" does not hold" 

and andToNL(Land(f1,f2))     = 
        let val left = case f1 of Land _ => toList(f1) | _ => toNL(f1)
            val right= case f2 of Land _ => ", "^andToNL(f2)
                                 | Lor _ => " holds and also "^toNL(f2)
                                     | _ => " and "^toNL(f2)^" holds"
        in left^right
        end
  | andToNL(_)              = "**errorA**"

and orToNL(Lor(f1,f2), s)   =
        let val left  = case f1 of Lor _ => toList(f1) | _ => toNL(f1)
            val right = case f2 of Lor _ => ", "^orToNL(f2,s) 
                                | Land _ => " holds"^s^"or "^toNL(f2)
                                |      _ => s^"or "^toNL(f2)^" holds"
        in left^right
        end
  | orToNL(_)               = "**errorO**"

and toList(Lor(f1,f2))  =
        let val left    = case f1 of Lor _ => toList(f1) | _ => toNL(f1)
            val right   = case f2 of Lor _ => toList(f2) | _ => toNL(f2) 
        in left^", "^right
        end
  | toList(Land(f1,f2)) =
        let val left    = case f1 of Land _ => toList(f1) | _ => toNL(f1)
            val right   = case f2 of Land _ => toList(f2) | _ => toNL(f2) 
        in left^", "^right
        end
  | toList( _ )         = "**errorL**"

and impToNL(f as Imp _)      = bracket(toNL(f))
  | impToNL(f as _)          = toNL(f);

(* Tests *)
local
val p = Atom #"p"
val q = Atom #"q"
val l = Atom #"l"
val r = Atom #"r"
val s = Atom #"s"
val test = []
(* Add new cases here at the top! *)
(* Examples from HR p.81 *)
val test = Land(Imp(p,q),Imp(Neg r, Lor(q, Land(Neg p, r))))::test (* l *)
val test = Imp(Lor(Imp(s,Lor(r,l)),Land(Neg q,r)),Imp(Neg(Imp(p,s)),r))::test
val test = Lor(s,Imp(Neg p, Neg p))::test (* j *)
val test = Imp(Lor(s, Neg p),Neg p)::test (* i *)
val test = Imp(Land(p,q),Lor(Neg r, Imp(q,r)))::test (* h *)
val test = Lor(Neg p, Imp(p,q))::test (* g *)
val test = Neg(Land(Land(Neg q, Imp(p,r)),Imp(r,q)))::test (* f *)
val test = Imp(p, Lor(Neg q, Imp(q,p)))::test (* e *)
val test = Land(p, Imp(Neg q, Neg p))::test (* d *)
val test = Imp(Land(p,Neg q), Neg p)::test (* c *)
(* ------- *)
val test = Imp(p, Imp(q,r))::test
val test = Imp(Imp(p,q),r)::test
val test = Land(p, Lor(q,r))::test
val test = Lor(Land(p,q),r)::test
val test = Land(Lor(p,q),r)::test
val test = Lor(p, Land(q,r))::test
val test = Land(Land(p,q),r)::test
val test = Land(p, Land(q,r))::test
val test = Neg(Neg p)::test
val test = Imp(p,q)::test
val test = Lor(p,q)::test
val test = Land(p,q)::test
val test = Neg(q)::test
val test = p::test
in
val test_toString = map printl (map toString test)
val test_toNL = map printl (map toNL test)
end;
quit();
