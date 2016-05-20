val proofI = Proof(
"Invalid",
Sequent([AND(IMP(Atom #"u",Atom #"t"),IMP(Atom #"u",Atom #"s"))],
         IMP(Atom #"u", AND(Atom #"t",Atom #"s"))), 
[   Step(SOME (AND(IMP(Atom #"u",Atom #"t"),Atom #"u")),
         Prm, [], "1"),
    Step(SOME (IMP (Atom #"u",Atom #"t")), Ae1,[Line "1"], "2"),
    Step(SOME (IMP (Atom #"u",Atom #"s")), Ae2,[Line "1"], "3"),
    Step(SOME (Atom #"u"), Ass,[], "4"),
    Step(SOME (Atom #"t"), Iel,[Line "4", Line "2"], "5"),
    Step(SOME (Atom #"s"), Iel,[Line "4", Line "3"], "6"),
    Step(SOME (AND (Atom #"t",Atom #"s")), 
         Ain,[Line "5", Line "6"], "7"),
    Step(NONE, Dis,[Line "4"], ""),
    Step(SOME (IMP(Atom #"u", AND(Atom #"t",Atom #"s"))), 
         Iin,[Box ("4", "7")], "")
]);
