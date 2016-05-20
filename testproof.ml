use "proofs.ml";

val p = Atom #"p";
val q = Atom #"q";
val r = Atom #"r";
val prm = AND(IMP(p,q),IMP(p,r));
val con = IMP(p, AND(q,r));
val seq = Sequent([prm], con);
val steps = [
Step(SOME prm, Prm, [], "prm"),
Step(SOME (IMP (p,q)), Ae1,[Line "prm"], "piq"),
Step(SOME (IMP (p,r)), Ae2,[Line "prm"], "pir"),
Step(SOME p, Ass,[], "p"),
Step(SOME q, Iel,[Line "p", Line "piq"], "q"),
Step(SOME r, Iel,[Line "p", Line "pir"], "r"),
Step(SOME (AND (q,r)), Ain,[Line "q", Line "r"], "qar"),
Step(NONE, Dis,[Line "p"], ""),
Step(SOME con, Iin,[Box ("p", "qar")], "")];
val proof = Proof("ex121s",seq, steps);
