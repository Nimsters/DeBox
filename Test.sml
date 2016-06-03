structure Test = struct
open Unittest;

local
fun results ([], number, failed) =     
    let val fails   = Int.toString(failed)
        val num     = Int.toString(number)
        val line1   = StringCvt.padRight #" " 70 ("Tests run: "^num)
    in 
        TextIO.output(TextIO.stdOut, line1^"\nTests failed: "^fails^"\n")
    end
  | results ((name, value)::rest,number, failed) =
    let val failed  = if value then failed else (failed + 1)
        val number  = number + 1
        val num     = Int.toString(number)
        val result  = Bool.toString(value)
        val line    = 
            StringCvt.padRight #"." 65 ("Test "^num^", "^name^": ")
        val next    = case value of true => "\r" | false => "\n"
    in
        (TextIO.output(TextIO.stdOut, line^result^next); 
        results (rest, number, failed))
    end
  

fun add (name, test, expected, tests) =
    (name, (test = expected))::tests

val log = TextIO.openOut("unittest_log.txt")
val p           = Atom "p"
val q           = Atom "q"
val f_and       = AND(p,q)
val f_or        = OR(p,q)
val f_bot       = BOT
val f_imp       = IMP(p,q)
val f_neg       = NEG p
val pV_lists    = (Line "blah", q)::[(Line "assumption", p)]
val pV_context  = ([], pV_lists, pV_lists, [Line "assumption"])
val pV_box      = (Box ("assumption", "blah"), f_imp)
val sV_step_T   = (NONE, Dis, [Line "assumption"], "")
val sV_step_F   = (NONE, Dis, [Line "meh"], "")

val tests       = []
val tests       = add ("Aux Member - Empty list", 
                        member(1,[]), false, tests)
val tests       = add ("Aux Member - Is member", 
                        member(1,[2,1]), true, tests)
val tests       = add ("Aux Member - Not member", 
                        member(1,[2,3]), false, tests)
val tests       = add ("Aux Feedback - True", 
                        feedback(true, log, "This should not show in log\n"),
                        true, tests)
val tests       = add ("Aux Feedback - false", 
                        feedback(false, log, 
                        "Feedback test for false input\n"), false, tests)
val tests       = add ("Aux Get prefix - True/Empty", 
                        getPrefix(true, ""), "\n[Conclusion]: ", tests)
val tests       = add ("Aux Get prefix - True/Self", 
                        getPrefix(true, "Test"), "\n[Test]: ", tests)
val tests       = add ("Aux Get prefix - False", 
                        getPrefix(false, "False"), " ", tests)
val tests       = add ("Basic Pattern - Prm, valid",
                        rulePattern(Prm, [], "Prm, valid: ", log), 
                        true, tests)
val tests       = add ("Basic Pattern - Prm, invalid",
                        rulePattern(Prm, [Line "Ref"],
                        "Prm, invalid: ",log), false, tests)
val tests       = add ("Basic Pattern - Ass, valid",
                        rulePattern(Ass, [], "Ass, valid: ", log), 
                        true, tests)
val tests       = add ("Basic Pattern - Ass, invalid",
                        rulePattern(Ass, [Line "Ref"],
                        "\nAss, invalid: ",log), false, tests)
val tests       = add ("Basic Pattern - Cpy, valid",
                        rulePattern(Cpy, [Line "Ref"], "Cpy, valid: ", log), 
                        true, tests)
val tests       = add ("Basic Pattern - Cpy, invalid",
                        rulePattern(Cpy, [],
                        "\nCpy, invalid: ",log), false, tests)
val tests       = add ("Basic Pattern - Dis, valid",
                        rulePattern(Dis, [Line "Ref"], "Dis, valid: ", log), 
                        true, tests)
val tests       = add ("Basic Pattern - Dis, invalid",
                        rulePattern(Dis, [],
                        "\nDis, invalid: ",log), false, tests)
val tests       = add ("Basic Pattern - Lem, valid",
                        rulePattern(Lem, [], "Lem, valid: ", log), 
                        true, tests)
val tests       = add ("Basic Pattern - Lem, invalid",
                        rulePattern(Lem, [Line "Ref"],
                        "\nLem, invalid: ",log), false, tests)
val tests       = add ("Basic Pattern - Ae1, valid",
                        rulePattern(Ae1, [Line "Ref"], "Ae1, valid: ", log), 
                        true, tests)
val tests       = add ("Basic Pattern - Ae1, invalid",
                        rulePattern(Ae1, [],
                        "\nAe1, invalid: ",log), false, tests)
val tests       = add ("Basic Pattern - Ae2, valid",
                        rulePattern(Ae2, [Line "Ref"], "Ae2, valid: ", log), 
                        true, tests)
val tests       = add ("Basic Pattern - Ae2, invalid",
                        rulePattern(Ae2, [],
                        "\nAe2, invalid: ",log), false, tests)
val tests       = add ("Basic Pattern - Oi1, valid",
                        rulePattern(Oi1, [Line "Ref"], "Oi1, valid: ", log), 
                        true, tests)
val tests       = add ("Basic Pattern - Oi1, invalid",
                        rulePattern(Oi1, [],
                        "\nOi1, invalid: ",log), false, tests)
val tests       = add ("Basic Pattern - Oi2, valid",
                        rulePattern(Oi2, [Line "Ref"], "Oi2, valid: ", log), 
                        true, tests)
val tests       = add ("Basic Pattern - Oi2, invalid",
                        rulePattern(Oi2, [],
                        "\nOi2, invalid: ",log), false, tests)
val tests       = add ("Basic Pattern - Din, valid",
                        rulePattern(Din, [Line "Ref"], "Din, valid: ", log), 
                        true, tests)
val tests       = add ("Basic Pattern - Din, invalid",
                        rulePattern(Din, [],
                        "\nDin, invalid: ",log), false, tests)
val tests       = add ("Basic Pattern - Del, valid",
                        rulePattern(Del, [Line "Ref"], "Del, valid: ", log), 
                        true, tests)
val tests       = add ("Basic Pattern - Del, invalid",
                        rulePattern(Del, [],
                        "\nDel, invalid: ",log), false, tests)
val tests       = add ("Basic Pattern - Bel, valid",
                        rulePattern(Bel, [Line "Ref"], "Bel, valid: ", log), 
                        true, tests)
val tests       = add ("Basic Pattern - Bel, invalid",
                        rulePattern(Bel, [],
                        "\nBel, invalid: ",log), false, tests)
val tests       = add ("Basic Pattern - Mod, valid",
                        rulePattern(Mod, [Line "Ref", Line "Ref2"], 
                        "Mod, valid: ", log), true, tests)
val tests       = add ("Basic Pattern - Mod, invalid",
                        rulePattern(Mod, [Line "Ref", Box ("Ref","Ref2")],
                        "\nMod, invalid: ",log), false, tests)
val tests       = add ("Basic Pattern - Ain, valid",
                        rulePattern(Ain, [Line "Ref", Line "Ref2"], 
                        "Ain, valid: ", log), true, tests)
val tests       = add ("Basic Pattern - Ain, invalid",
                        rulePattern(Ain, [Line "Ref", Box ("Ref","Ref2")],
                        "\nAin, invalid: ",log), false, tests)
val tests       = add ("Basic Pattern - Iel, valid",
                        rulePattern(Iel, [Line "Ref", Line "Ref2"], 
                        "Iel, valid: ", log), true, tests)
val tests       = add ("Basic Pattern - Iel, invalid",
                        rulePattern(Iel, [Line "Ref", Box ("Ref","Ref2")],
                        "\nIel, invalid: ",log), false, tests)
val tests       = add ("Basic Pattern - Nel, valid",
                        rulePattern(Nel, [Line "Ref", Line "Ref2"], 
                        "Nel, valid: ", log), true, tests)
val tests       = add ("Basic Pattern - Nel, invalid",
                        rulePattern(Nel, [Line "Ref", Box ("Ref","Ref2")],
                        "\nNel, invalid: ",log), false, tests)
val tests       = add ("Basic Pattern - Iin, valid",
                        rulePattern(Iin, [Box ("Ref","Ref2")], 
                        "Iin, valid: ", log), true, tests)
val tests       = add ("Basic Pattern - Iin, invalid",
                        rulePattern(Iin, [Line "Ref", Box ("Ref","Ref2")],
                        "\nIin, invalid: ",log), false, tests)
val tests       = add ("Basic Pattern - Nin, valid",
                        rulePattern(Nin, [Box ("Ref","Ref2")], 
                        "Nin, valid: ", log), true, tests)
val tests       = add ("Basic Pattern - Nin, invalid",
                        rulePattern(Nin, [Line "Ref", Box ("Ref","Ref2")],
                        "\nNin, invalid: ",log), false, tests)
val tests       = add ("Basic Pattern - Pbc, valid",
                        rulePattern(Pbc, [Box ("Ref","Ref2")], 
                        "Pbc, valid: ", log), true, tests)
val tests       = add ("Basic Pattern - Pbc, invalid",
                        rulePattern(Pbc, [Line "Ref", Box ("Ref","Ref2")],
                        "\nPbc, invalid: ",log), false, tests)
val tests       = add ("Basic Pattern - Oel, valid", rulePattern(Oel, [
                        Line "Ref", Box ("Ref","Ref2"), Box ("Ass", "Con")], 
                        "Oel, valid: ", log), true, tests)
val tests       = add ("Basic Pattern - Oel, invalid",
                        rulePattern(Oel, [Line "Ref", Box ("Ref","Ref2")],
                        "\nOel, invalid: ",log), false, tests)

val tests       = add ("PatVal Dis", 
                       patternValidation( (NONE, Dis, [p]), 
                        [Line "assumption"],pV_context, true, "", log),
                       (true, ([], pV_box::pV_lists, [pV_box], [])),
                       tests)
val tests       = add ("IDVal valid", idValidation("grr", pV_lists, log),
                       true, tests)
val tests       = add ("IDVal invalid",idValidation("blah", pV_lists, log),
                       false, tests)
in
    val test = (TextIO.closeOut(log); results (rev tests, 0, 0))
end;
end;
