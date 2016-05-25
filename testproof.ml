use "proofs.ml";

val p = Atom "p";
val q = Atom "q";
val r = Atom "r";
val prm = AND(IMP(p,q),IMP(p,r));
val con = IMP(p, AND(q,r));
val seq = ([prm], con);
val steps = [
(SOME prm, Prm, [], "prm"),
(SOME (IMP (p,q)), Ae1,[Line "prm"], "piq"),
(SOME (IMP (p,r)), Ae2,[Line "prm"], "pir"),
(SOME p, Ass,[], "p"),
(SOME q, Iel,[Line "p", Line "piq"], "q"),
(SOME r, Iel,[Line "p", Line "pir"], "r"),
(SOME (AND (q,r)), Ain,[Line "q", Line "r"], "qar"),
(NONE, Dis,[Line "p"], ""),
(SOME con, Iin,[Box ("p", "qar")], "")];
val proof = ("ex121s",seq, steps);
