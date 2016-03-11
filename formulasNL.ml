datatype formula = Atom of char |
                   NEG of formula |
                   AND of formula*formula |
                   OR of formula*formula |
                   IMP of formula*formula;

fun bracket(s) = "("^s^")";

fun toString(Atom c)        = Char.toString c
  | toString(NEG f)         = "¬"^negToString(f)
  | toString(AND (f1,f2))  = andToString(f1)^"/\\"^andToString(f2)
  | toString(OR (f1, f2))  = impToString(f1)^"\\/"^impToString(f2)
  | toString(IMP (f1, f2))  = impToString(f1)^"=>"^toString(f2)

and negToString(a as Atom _)    = toString(a)
  | negToString(n as NEG _)     = toString(n)
  | negToString(f as _)         = bracket(toString(f))

and andToString(f as OR _)     = bracket(toString(f))
  | andToString(f as IMP _)     = bracket(toString(f))
  | andToString(f as _)         = toString(f)

and impToString(f as IMP _)      = bracket(toString(f))
  | impToString(f as _)          = toString(f);

fun printl s = print(s^"\n");

fun toNL(Atom c)        = Char.toString c
  | toNL(NEG f)         = negToNL(f)
  | toNL(f as AND _)   = andToNL(f)
  | toNL(f as OR _)     = orToNL(f, " ")
  | toNL(IMP (f1, f2))  = impToNL(f1)^" implies that "^toNL(f2)

and negToNL(a as Atom _) = "not-"^toNL(a)
  | negToNL(f as AND _) = "not all of "^andToNL(f)
  | negToNL(f as OR _)  = "neither "^orToNL(f, " n")
  | negToNL(f as NEG _)       = 
        let val n      = "not-"
            val result = toNL(f)
        in
            if (String.isPrefix n result) 
            then n^result
            else "it is not the case that "^result
        end
  | negToNL(f as IMP _)  = "the implication \""^impToNL(f)^"\" does not hold" 

and andToNL(AND(f1,f2))     = 
        let val left = case f1 of AND _ => toList(f1) | _ => toNL(f1)
            val right= case f2 of AND _ => ", "^andToNL(f2)
                                 | OR _ => " holds and also "^toNL(f2)
                                     | _ => " and "^toNL(f2)^" holds"
        in left^right
        end
  | andToNL(_)              = "**errorA**"

and orToNL(OR(f1,f2), s)   =
        let val left  = case f1 of OR _ => toList(f1) | _ => toNL(f1)
            val right = case f2 of OR _ => ", "^orToNL(f2,s) 
                                | AND _ => " holds"^s^"or "^toNL(f2)
                                |      _ => s^"or "^toNL(f2)^" holds"
        in left^right
        end
  | orToNL(_)               = "**errorO**"

and toList(OR(f1,f2))  =
        let val left    = case f1 of OR _ => toList(f1) | _ => toNL(f1)
            val right   = case f2 of OR _ => toList(f2) | _ => toNL(f2) 
        in left^", "^right
        end
  | toList(AND(f1,f2)) =
        let val left    = case f1 of AND _ => toList(f1) | _ => toNL(f1)
            val right   = case f2 of AND _ => toList(f2) | _ => toNL(f2) 
        in left^", "^right
        end
  | toList( _ )         = "**errorL**"

and impToNL(f as IMP _)      = bracket(toNL(f))
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
val test = AND(IMP(p,q),IMP(NEG r, OR(q, AND(NEG p, r))))::test (* l *)
val test = IMP(OR(IMP(s,OR(r,l)),AND(NEG q,r)),IMP(NEG(IMP(p,s)),r))::test
val test = OR(s,IMP(NEG p, NEG p))::test (* j *)
val test = IMP(OR(s, NEG p),NEG p)::test (* i *)
val test = IMP(AND(p,q),OR(NEG r, IMP(q,r)))::test (* h *)
val test = OR(NEG p, IMP(p,q))::test (* g *)
val test = NEG(AND(AND(NEG q, IMP(p,r)),IMP(r,q)))::test (* f *)
val test = IMP(p, OR(NEG q, IMP(q,p)))::test (* e *)
val test = AND(p, IMP(NEG q, NEG p))::test (* d *)
val test = IMP(AND(p,NEG q), NEG p)::test (* c *)
(* ------- *)
val test = NEG(IMP(p, IMP(q,r)))::test
val test = NEG(IMP(IMP(p,q),r))::test
val test = NEG(AND(p, OR(q,r)))::test
val test = NEG(OR(AND(p,q),r))::test
val test = NEG(AND(OR(p,q),r))::test
val test = NEG(OR(p, AND(q,r)))::test
val test = NEG(AND(AND(p,q),r))::test
val test = NEG(AND(p, AND(q,r)))::test
val test = IMP(p, IMP(q,r))::test
val test = IMP(IMP(p,q),r)::test
val test = AND(p, OR(q,r))::test
val test = OR(AND(p,q),r)::test
val test = AND(OR(p,q),r)::test
val test = OR(p, AND(q,r))::test
val test = AND(AND(p,q),r)::test
val test = AND(p, AND(q,r))::test
val test = NEG(NEG p)::test
val test = IMP(p,q)::test
val test = OR(p,q)::test
val test = AND(p,q)::test
val test = NEG(q)::test
val test = p::test
in
val test_toString = map printl (map toString test)
val test_toNL = map printl (map toNL test)
end;
quit();
