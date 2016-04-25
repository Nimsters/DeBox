use "proofs.ml";

val p = Atom #"p";
val q = Atom #"q";
val r = Atom #"r";
val prm = AND(IMP(p,q),IMP(p,r));
val con = IMP(p, AND(q,r));
val seq = Sequent([prm], con);
val steps = [
Step(prm, Prm, [], Line "prm"),
Step(IMP (p,q), Ae1,[Line "prm"], Line "piq"),
Step(IMP (p,r), Ae2,[Line "prm"], Line "pir"),
Step(p, Ass,[], Line "p"),
Step(q, Iel,[Line "p", Line "piq"], Line "q"),
Step(r, Iel,[Line "p", Line "pir"], Line "r"),
Step(AND(q,r), Ain,[Line "q", Line "r"],Box ("p", "qar")),
Step(con, Iin,[Box ("p", "qar")], Conclusion)];
val proof = Proof("ex121s",seq, steps);
