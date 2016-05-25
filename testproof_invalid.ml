val proofI = (
"Invalid",
([AND(IMP(Atom "u",Atom "t"),IMP(Atom "u",Atom "s"))],
         IMP(Atom "u", AND(Atom "t",Atom "s"))), 
[   (SOME (AND(IMP(Atom "u",Atom "t"),Atom "u")),
         Prm, [], "1"),
    (SOME (IMP (Atom "u",Atom "t")), Ae1,[Line "1"], "2"),
    (SOME (IMP (Atom "u",Atom "s")), Ae2,[Line "1"], "3"),
    (SOME (Atom "u"), Ass,[], "4"),
    (SOME (Atom "t"), Iel,[Line "4", Line "2"], "5"),
    (SOME (Atom "s"), Iel,[Line "4", Line "3"], "6"),
    (SOME (AND (Atom "t",Atom "s")), 
         Ain,[Line "5", Line "6"], "7"),
    (NONE, Dis,[Line "4"], ""),
    (SOME (IMP(Atom "u", AND(Atom "t",Atom "s"))), 
         Iin,[Box ("4", "7")], "")
]);
