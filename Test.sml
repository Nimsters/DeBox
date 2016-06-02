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
                        feedback(false, log, "Feedback test false\n"),
                        false, tests)
val tests       = add ("Aux Get prefix - True/Empty", 
                        getPrefix(true, ""), "\n[Conclusion]: ", tests)
val tests       = add ("Aux Get prefix - True/Self", 
                        getPrefix(true, "Test"), "\n[Test]: ", tests)
val tests       = add ("Aux Get prefix - False", 
                        getPrefix(false, "False"), " ", tests)
val tests       = add ("Basic Pattern - Lem, valid",
                        rulePattern(Lem, [], "Lem, valid: ", log), 
                        true, tests)
val tests       = add ("Basic Pattern - Lem, invalid",
                        rulePattern(Lem, [Line "Ref"],"Lem, invalid: ",log), 
                        false, tests)
val tests       = add ("Basic Pattern - Ass, valid",
                        rulePattern(Ass, [], "Ass, valid: ", log), 
                        true, tests)
val tests       = add ("Basic Pattern - Ass, invalid",
                        rulePattern(Ass, [Line "Ref"],"\nAss, invalid: ",
                        log), false, tests)
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
