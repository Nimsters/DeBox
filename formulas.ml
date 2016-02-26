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
val test = Land(Imp(p,q),Imp(Neg r, Lor(q, Land(Neg p, r))))::test
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
end;
quit();
