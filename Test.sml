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
val p   = Atom "p"
val q   = Atom "q"
val pV_lists    = (Line "blah", q)::[(Line "ass", p)]
val pV_context  = ([], pV_lists, pV_lists, [Line "ass"])
val pV_box      = (Box ("ass", "blah"), IMP(p,q))
val sV_step_T   = (NONE, Dis, [Line "ass"], "")
val sV_step_F   = (NONE, Dis, [Line "meh"], "")

val tests       = []
val tests       = add ("PatVal Dis", 
                       patternValidation( (NONE, Dis, [p]), 
                        [Line "ass"],pV_context, true, "", log),
                       (true, ([], pV_box::pV_lists, [pV_box], [])),
                       tests)
val tests       = add ("IDVal valid", idValidation("grr", pV_lists, log),
                       true, tests)
val tests       = add ("IDVal invalid",idValidation("blah", pV_lists, log),
                       false, tests)
in
    val test = results (rev tests, 0, 0)
end;
end;
