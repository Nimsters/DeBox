use "formulasNL.ml";

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
val test = OR(OR(p,q),r)::test
val test = OR(p, OR(q,r))::test
val test = AND(AND(p,q),r)::test
val test = AND(p, AND(q,r))::test
val test = NEG(NEG p)::test
val test = IMP(p,q)::test
val test = OR(p,q)::test
val test = AND(p,q)::test
val test = NEG(q)::test
val test = p::test
val t0::t1::rest = test
in
val test_toString = map printl (map toString test)
(* val test_toNL = map printl (map toNL test) 
val test_toProp = map toProp test*)
end;
quit();
