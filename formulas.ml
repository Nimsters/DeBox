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
end;
quit();
