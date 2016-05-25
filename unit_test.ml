use "proofs.ml";

local
val out = TextIO.stdOut
val log = TextIO.openOut("unittest_log.txt")
val p   = Atom "p"
val q   = Atom "q"
val pV_lists    = (Line "blah", q)::[(Line "ass", p)]
val pV_context  = ([], pV_lists, pV_lists, [Line "ass"])
val pV_box      = (Box ("ass", "blah"), IMP(p,q))
val sV_step_T   = (NONE, Dis, [Line "ass"], "")
val sV_step_F   = (NONE, Dis, [Line "meh"], "")
val _ = print "\n"
in
val patVal_dis__T   = (patternValidation(
    (NONE, Dis, [p]), [Line "ass"],pV_context, true, "", out) =
    (true, ([], pV_box::pV_lists, [pV_box], [])))
val idVal_______T   = idValidation("", pV_lists, out)
val idVal_______F   = not (idValidation("blah", pV_lists, log))
val ruleVal_dis_T   = 
    case ruleValidation(true, pV_context, sV_step_T, log) of
    (b, _) => b
val stepVal_dis_T   = 
    case stepValidation(true, pV_context, log, [sV_step_T]) of
    (b, _, _) => b
end;
quit();
