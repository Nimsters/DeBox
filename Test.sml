structure Test = struct
open Unittest;

local
fun results ([], number, failed, _) =     
    let val fails   = Int.toString(failed)
        val num     = Int.toString(number)
        val line1   = StringCvt.padRight #" " 70 ("Tests run: "^num)
    in 
        TextIO.output(TextIO.stdOut, line1^"\nTests failed: "^fails^"\n")
    end
  | results ((name, value)::rest,number, failed, return) =
    let val failed  = if value then failed else (failed + 1)
        val number  = number + 1
        val num     = Int.toString(number)
        val result  = Bool.toString(value)
        val line    = 
            StringCvt.padRight #"." 65 ("Test "^num^", "^name^": ")
        val next    = case value of true => return | false => "\n"
    in
        (TextIO.output(TextIO.stdOut, line^result^next); 
        results (rest, number, failed, return))
    end
  

fun add (name, test, expected, tests) =
    (name, (test = expected))::tests

val log = TextIO.openOut("unittest_log.txt")
val p           = Atom "p"
val q           = Atom "q"
val r           = Atom "r"
val n_p         = NEG p
val n_q         = NEG q
val n_r         = NEG r
val f_and       = AND(p,q)
val f_or        = OR(p,q)
val f_bot       = BOT
val f_imp       = IMP(p,q)
val f_lem       = OR(r, n_r)
val f_dbl       = NEG(NEG p)
val b_pir       = IMP(p, r)
val b_qir       = IMP(q, r)
val b_pbc       = IMP(n_r, f_bot)
val b_neg       = IMP(p, f_bot)
val pV_lists    = rev [
                    (Line "p", p), (Line "q", q), (Line "r", r),
                    (Line "p_and_q", f_and), (Line "p_or_q", f_or),
                    (Line "abs", f_bot), (Line "p_implies_q", f_imp),
                    (Box ("not_r","abs"), b_pbc), (Box ("p", "abs"), b_neg),
                    (Box ("p", "r"), b_pir), (Box ("q", "r"), b_qir),
                    (Line "dbl_neg_p", f_dbl), (Line "not_r", n_r) 
                  ]
val pV_context0 = ([p, q], pV_lists, pV_lists, [])
val pV_context1 = ([], pV_lists, pV_lists, [Line "p"])
val pV_context2 = ([], pV_lists, pV_lists, [Line "not_r", Line "p"])
val pV_dis_box  = (Box ("p","not_r"), IMP(p, n_r))
val pV_n_r_box  = (Box ("not_r", "not_r"), IMP(n_r, n_r))
val sV_step_T   = (NONE, Dis, [Line "assumption"], "")
val sV_step_F   = (NONE, Dis, [Line "meh"], "")
val proof_val   =
("unittest_valid",
([AND(IMP(p,q),IMP(p,r))],
    IMP(p,AND(q,r))),
[(SOME(AND(IMP(p,q),IMP(p,r))),Prm,[],"prm"),
 (SOME(IMP(p,q)),Ae1,[Line("prm")],"piq"),
 (SOME(IMP(p,r)),Ae2,[Line("prm")],"pir"),
 (SOME(p),Ass,[],"p"),
 (SOME(q),Iel,[Line("p"),Line("piq")],"q"),
 (SOME(r),Iel,[Line("p"),Line("pir")],"r"),
 (SOME(AND(q,r)),Ain,[Line("q"),Line("r")],"qar"),
 (NONE,Dis,[Line("p")],""),
 (SOME(IMP(p,AND(q,r))),Iin,[Box("p","qar")],"")])
val proof_inv   =
("unittest_invalid",
([AND(IMP(p,q),IMP(p,r))],
    IMP(p,AND(q,r))),
[(SOME(AND(IMP(p,q),IMP(p,r))),Prm,[],"prm"),
 (SOME(IMP(p,q)),Ae1,[Line("prm")],"prm"),
 (SOME(IMP(p,r)),Ae2,[Line("prm")],"pir"),
 (SOME(p),Ass,[],"p"),
 (SOME(q),Iel,[Line("p"),Line("pir")],"q"),
 (SOME(r),Iel,[Line("p"),Line("pir")],"r"),
 (SOME(AND(q,r)),Ain,[Line("q")],"qar"),
 (NONE,Dis,[Line("q")],""),
 (SOME(IMP(p,AND(q,r))),Iin,[Box("p","qar")],"")])

val tests       = []
(* Tests of auxiliary functions *)
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
(* Basic pattern tests *)
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
(* Specific pattern tests *)
val tests       = add ("Specific pattern - Prm, valid", 
                        patternValidation( (SOME p, Prm, []), 
                            [], pV_context0, true, "prm_p", log),
                       (true,([q], (Line "prm_p", p)::pV_lists ,
                            (Line "prm_p", p)::pV_lists, [])),
                        tests)
val tests       = add ("Specific pattern - Prm, not first premise", 
                        patternValidation( (SOME q, Prm, []), 
                            [], pV_context0, true, "prm_q", log),
                       (false,([p], (Line "prm_q", q)::pV_lists ,
                            (Line "prm_q", q)::pV_lists, [])),
                        tests)
val tests       = add ("Specific pattern - Prm, empty premise list",
                        patternValidation( (SOME q, Prm, []), 
                            [], pV_context1, true, "prm_q", log),
                       (false,([], (Line "prm_q", q)::pV_lists ,
                            (Line "prm_q", q)::pV_lists, [Line "p"])),
                        tests)
val tests       = add ("Specific pattern - Ass, valid",
                        patternValidation( (SOME q, Ass, []), 
                            [], pV_context1, true, "ass_q", log),
                       (true,([], (Line "ass_q", q)::pV_lists ,
                            (Line "ass_q", q)::pV_lists, 
                                [Line "ass_q",Line "p"])),
                        tests)
val tests       = add ("Specific pattern - Dis, valid", 
                       patternValidation( (NONE, Dis, [p]), 
                            [Line "p"],pV_context1, true, "", log),
                       (true, ([], pV_dis_box::pV_lists, [pV_dis_box], [])),
                       tests)
val tests       = add ("Specific pattern - Cpy, valid", 
                       patternValidation( (SOME p, Cpy, [p]), 
                            [],pV_context1, true, "cpy_p", log),
                       (true, ([], (Line "cpy_p", p)::pV_lists, 
                            (Line "cpy_p", p)::pV_lists, [Line "p"])),
                       tests)
val tests       = add ("Specific pattern - Cpy, wrong referent", 
                       patternValidation( (SOME p, Cpy, [q]), 
                            [],pV_context1, true, "cpy_p", log),
                       (false, ([], (Line "cpy_p", p)::pV_lists, 
                            (Line "cpy_p", p)::pV_lists, [Line "p"])),
                       tests)
val tests       = add ("Specific pattern - Ain, valid", 
                       patternValidation( (SOME f_and, Ain, [p,q]), 
                            [],pV_context1, true, "p_and_q", log),
                       (true, ([], (Line "p_and_q", f_and)::pV_lists, 
                            (Line "p_and_q", f_and)::pV_lists, [Line "p"])),
                       tests)
val tests       = add ("Specific pattern - Ain, not AND", 
                       patternValidation( (SOME f_or, Ain, [p,q]), 
                            [],pV_context1, true, "p_and_q", log),
                       (false, ([], (Line "p_and_q", f_or)::pV_lists, 
                            (Line "p_and_q", f_or)::pV_lists, [Line "p"])),
                       tests)
val tests       = add ("Specific pattern - Ain, wrong conjuncts", 
                       patternValidation( (SOME f_and, Ain, [q,p]), 
                            [],pV_context1, true, "p_and_q", log),
                       (false, ([], (Line "p_and_q", f_and)::pV_lists, 
                            (Line "p_and_q", f_and)::pV_lists, [Line "p"])),
                       tests)
val tests       = add ("Specific pattern - Ae1, valid", 
                       patternValidation( (SOME p, Ae1, [f_and]), 
                            [],pV_context1, true, "and_e1", log),
                       (true, ([], (Line "and_e1", p)::pV_lists, 
                            (Line "and_e1", p)::pV_lists, [Line "p"])),
                       tests)
val tests       = add ("Specific pattern - Ae1, not AND", 
                       patternValidation( (SOME p, Ae1, [f_or]), 
                            [],pV_context1, true, "and_e1", log),
                       (false, ([], (Line "and_e1", p)::pV_lists, 
                            (Line "and_e1", p)::pV_lists, [Line "p"])),
                       tests)
val tests       = add ("Specific pattern - Ae1, wrong referent", 
                       patternValidation( (SOME q, Ae1, [f_and]), 
                            [],pV_context1, true, "and_e1", log),
                       (false, ([], (Line "and_e1", q)::pV_lists, 
                            (Line "and_e1", q)::pV_lists, [Line "p"])),
                       tests)
val tests       = add ("Specific pattern - Ae2, valid", 
                       patternValidation( (SOME q, Ae2, [f_and]), 
                            [],pV_context1, true, "and_e2", log),
                       (true, ([], (Line "and_e2", q)::pV_lists, 
                            (Line "and_e2", q)::pV_lists, [Line "p"])),
                       tests)
val tests       = add ("Specific pattern - Ae2, not AND", 
                       patternValidation( (SOME q, Ae2, [f_or]), 
                            [],pV_context1, true, "and_e2", log),
                       (false, ([], (Line "and_e2", q)::pV_lists, 
                            (Line "and_e2", q)::pV_lists, [Line "p"])),
                       tests)
val tests       = add ("Specific pattern - Ae2, wrong referent", 
                       patternValidation( (SOME p, Ae2, [f_and]), 
                            [],pV_context1, true, "and_e2", log),
                       (false, ([], (Line "and_e2", p)::pV_lists, 
                            (Line "and_e2", p)::pV_lists, [Line "p"])),
                       tests)
val tests       = add ("Specific pattern - Oi1, valid", 
                       patternValidation( (SOME f_or, Oi1, [p]), 
                            [],pV_context1, true, "or_i1", log),
                       (true, ([], (Line "or_i1", f_or)::pV_lists, 
                            (Line "or_i1", f_or)::pV_lists, [Line "p"])),
                       tests)
val tests       = add ("Specific pattern - Oi1, not OR", 
                       patternValidation( (SOME p, Oi1, [q]), 
                            [],pV_context1, true, "or_i1", log),
                       (false, ([], (Line "or_i1", p)::pV_lists, 
                            (Line "or_i1", p)::pV_lists, [Line "p"])),
                       tests)
val tests       = add ("Specific pattern - Oi1, wrong referent", 
                       patternValidation( (SOME f_or, Oi1, [q]), 
                            [],pV_context1, true, "or_i1", log),
                       (false, ([], (Line "or_i1", f_or)::pV_lists, 
                            (Line "or_i1", f_or)::pV_lists, [Line "p"])),
                       tests)
val tests       = add ("Specific pattern - Oi2, valid", 
                       patternValidation( (SOME f_or, Oi2, [q]), 
                            [],pV_context1, true, "or_i2", log),
                       (true, ([], (Line "or_i2", f_or)::pV_lists, 
                            (Line "or_i2", f_or)::pV_lists, [Line "p"])),
                       tests)
val tests       = add ("Specific pattern - Oi2, not OR", 
                       patternValidation( (SOME q, Oi2, [p]), 
                            [],pV_context1, true, "or_i2", log),
                       (false, ([], (Line "or_i2", q)::pV_lists, 
                            (Line "or_i2", q)::pV_lists, [Line "p"])),
                       tests)
val tests       = add ("Specific pattern - Oi2, wrong referent", 
                       patternValidation( (SOME f_or, Oi2, [p]), 
                            [],pV_context1, true, "or_i1", log),
                       (false, ([], (Line "or_i1", f_or)::pV_lists, 
                            (Line "or_i1", f_or)::pV_lists, [Line "p"])),
                       tests)
val tests       = add ("Specific pattern - Iel, valid", 
                       patternValidation( (SOME q, Iel, [p, f_imp]), 
                            [],pV_context1, true, "imp_el", log),
                       (true, ([], (Line "imp_el", q)::pV_lists, 
                            (Line "imp_el", q)::pV_lists, [Line "p"])),
                       tests)
val tests       = add ("Specific pattern - Iel, not IMP", 
                       patternValidation( (SOME q, Iel, [p, q]), 
                            [],pV_context1, true, "imp_el", log),
                       (false, ([], (Line "imp_el", q)::pV_lists, 
                            (Line "imp_el", q)::pV_lists, [Line "p"])),
                       tests)
val tests       = add ("Specific pattern - Iel, wrong referents", 
                       patternValidation( (SOME p, Iel, [q, f_imp]), 
                            [],pV_context1, true, "imp_el", log),
                       (false, ([], (Line "imp_el", p)::pV_lists, 
                            (Line "imp_el", p)::pV_lists, [Line "p"])),
                       tests)
val tests       = add ("Specific pattern - Nel, valid", 
                       patternValidation( (SOME f_bot, Nel, [p, NEG p]), 
                            [],pV_context1, true, "neg_el", log),
                       (true, ([], (Line "neg_el", f_bot)::pV_lists, 
                            (Line "neg_el", f_bot)::pV_lists, [Line "p"])),
                       tests)
val tests       = add ("Specific pattern - Nel, 2nd ref not neg of 1st", 
                       patternValidation( (SOME f_bot, Nel, [p, NEG q]), 
                            [],pV_context1, true, "neg_el", log),
                       (false, ([], (Line "neg_el", f_bot)::pV_lists, 
                            (Line "neg_el", f_bot)::pV_lists, [Line "p"])),
                       tests)
val tests       = add ("Specific pattern - Nel, not BOT", 
                       patternValidation( (SOME p, Nel, [q, f_bot]), 
                            [],pV_context1, true, "neg_el", log),
                       (false, ([], (Line "neg_el", p)::pV_lists, 
                            (Line "neg_el", p)::pV_lists, [Line "p"])),
                       tests)
val tests       = add ("Specific pattern - Din, valid", 
                       patternValidation( (SOME f_dbl, Din, [p]), 
                            [],pV_context1, true, "dbl_in", log),
                       (true, ([], (Line "dbl_in", f_dbl)::pV_lists, 
                            (Line "dbl_in", f_dbl)::pV_lists, [Line "p"])),
                       tests)
val tests       = add ("Specific pattern - Din, wrong referent", 
                       patternValidation( (SOME f_dbl, Din, [q]), 
                            [],pV_context1, true, "dbl_in", log),
                       (false, ([], (Line "dbl_in", f_dbl)::pV_lists, 
                            (Line "dbl_in", f_dbl)::pV_lists, [Line "p"])),
                       tests)
val tests       = add ("Specific pattern - Din, not double negation", 
                       patternValidation( (SOME p, Din, [f_dbl]), 
                            [],pV_context1, true, "dbl_in", log),
                       (false, ([], (Line "dbl_in", p)::pV_lists, 
                            (Line "dbl_in", p)::pV_lists, [Line "p"])),
                       tests)
val tests       = add ("Specific pattern - Del, valid", 
                       patternValidation( (SOME p, Del, [f_dbl]), 
                            [],pV_context1, true, "dbl_el", log),
                       (true, ([], (Line "dbl_el", p)::pV_lists, 
                            (Line "dbl_el", p)::pV_lists, [Line "p"])),
                       tests)
val tests       = add ("Specific pattern - Del, wrong referent", 
                       patternValidation( (SOME p, Del, [NEG(NEG q)]), 
                            [],pV_context1, true, "dbl_in", log),
                       (false, ([], (Line "dbl_in", p)::pV_lists, 
                            (Line "dbl_in", p)::pV_lists, [Line "p"])),
                       tests)
val tests       = add ("Specific pattern - Del, not double negation", 
                       patternValidation( (SOME p, Del, [q]), 
                            [],pV_context1, true, "dbl_in", log),
                       (false, ([], (Line "dbl_in", p)::pV_lists, 
                            (Line "dbl_in", p)::pV_lists, [Line "p"])),
                       tests)
val tests       = add ("Specific pattern - Bel, valid", 
                       patternValidation( (SOME q, Bel, [f_bot]), 
                            [],pV_context1, true, "bot_el", log),
                       (true, ([], (Line "bot_el", q)::pV_lists, 
                            (Line "bot_el", q)::pV_lists, [Line "p"])),
                       tests)
val tests       = add ("Specific pattern - Bel, not BOT", 
                       patternValidation( (SOME q, Bel, [p]), 
                            [],pV_context1, true, "bot_el", log),
                       (false, ([], (Line "bot_el", q)::pV_lists, 
                            (Line "bot_el", q)::pV_lists, [Line "p"])),
                       tests)
val tests       = add ("Specific pattern - Mod, valid", 
                       patternValidation( (SOME n_p, Mod, [f_imp, n_q]), 
                            [],pV_context1, true, "mod", log),
                       (true, ([], (Line "mod", n_p)::pV_lists, 
                            (Line "mod", n_p)::pV_lists, [Line "p"])),
                       tests)
val tests       = add ("Specific pattern - Mod, not IMP", 
                       patternValidation( (SOME q, Mod, [p, q]), 
                            [],pV_context1, true, "mod", log),
                       (false, ([], (Line "mod", q)::pV_lists, 
                            (Line "mod", q)::pV_lists, [Line "p"])),
                       tests)
val tests       = add ("Specific pattern - Mod, wrong referents", 
                       patternValidation( (SOME n_q, Mod, [f_imp, n_r]), 
                            [],pV_context1, true, "mod", log),
                       (false, ([], (Line "mod", n_q)::pV_lists, 
                            (Line "mod", n_q)::pV_lists, [Line "p"])),
                       tests)
val tests       = add ("Specific pattern - Lem, valid", 
                       patternValidation( (SOME f_lem, Lem, []), 
                            [],pV_context1, true, "lem", log),
                       (true, ([], (Line "lem", f_lem)::pV_lists, 
                            (Line "lem", f_lem)::pV_lists, [Line "p"])),
                       tests)
val tests       = add ("Specific pattern - Lem, not OR", 
                       patternValidation( (SOME q, Lem, []), 
                            [],pV_context1, true, "lem", log),
                       (false, ([], (Line "lem", q)::pV_lists, 
                            (Line "lem", q)::pV_lists, [Line "p"])),
                       tests)
val tests       = add ("Specific pattern - Lem, wrong referents", 
                       patternValidation( (SOME f_or, Lem, []), 
                            [],pV_context1, true, "lem", log),
                       (false, ([], (Line "lem", f_or)::pV_lists, 
                            (Line "lem", f_or)::pV_lists, [Line "p"])),
                       tests)
val tests       = add ("Specific pattern - Iin, valid", 
                       patternValidation( (SOME f_imp, Iin, [f_imp]), 
                            [],pV_context1, true, "imp_in", log),
                       (true, ([], (Line "imp_in", f_imp)::pV_lists, 
                            (Line "imp_in", f_imp)::pV_lists, [Line "p"])),
                       tests)
val tests       = add ("Specific pattern - Iin, not IMP", 
                       patternValidation( (SOME q, Iin, [f_imp]), 
                            [],pV_context1, true, "imp_in", log),
                       (false, ([], (Line "imp_in", q)::pV_lists, 
                            (Line "imp_in", q)::pV_lists, [Line "p"])),
                       tests)
val tests       = add ("Specific pattern - Iin, wrong referents", 
                       patternValidation( (SOME f_imp, Iin, [b_qir]), 
                            [],pV_context1, true, "imp_in", log),
                       (false, ([], (Line "imp_in", f_imp)::pV_lists, 
                            (Line "imp_in", f_imp)::pV_lists, [Line "p"])),
                       tests)
val tests       = add ("Specific pattern - Nin, valid", 
                       patternValidation( (SOME n_p, Nin, [b_neg]), 
                            [],pV_context1, true, "neg_in", log),
                       (true, ([], (Line "neg_in", n_p)::pV_lists, 
                            (Line "neg_in", n_p)::pV_lists, [Line "p"])),
                       tests)
val tests       = add ("Specific pattern - Nin, not NEG", 
                       patternValidation( (SOME p, Nin, [b_neg]), 
                            [],pV_context1, true, "neg_in", log),
                       (false, ([], (Line "neg_in", p)::pV_lists, 
                            (Line "neg_in", p)::pV_lists, [Line "p"])),
                       tests)
val tests       = add ("Specific pattern - Nin, wrong referents", 
                       patternValidation( (SOME n_p, Nin, [b_qir]), 
                            [],pV_context1, true, "neg_in", log),
                       (false, ([], (Line "neg_in", n_p)::pV_lists, 
                            (Line "neg_in", n_p)::pV_lists, [Line "p"])),
                       tests)
val tests       = add ("Specific pattern - Pbc, valid", 
                       patternValidation( (SOME r, Pbc, [b_pbc]), 
                            [],pV_context1, true, "pbc", log),
                       (true, ([], (Line "pbc", r)::pV_lists, 
                            (Line "pbc", r)::pV_lists, [Line "p"])),
                       tests)
val tests       = add ("Specific pattern - Pbc, not BOT", 
                       patternValidation( (SOME p, Pbc, [b_qir]), 
                            [],pV_context1, true, "pbc", log),
                       (false, ([], (Line "pbc", p)::pV_lists, 
                            (Line "pbc", p)::pV_lists, [Line "p"])),
                       tests)
val tests       = add ("Specific pattern - Pbc, wrong referents", 
                       patternValidation( (SOME p, Pbc, [b_pbc]), 
                            [],pV_context1, true, "pbc", log),
                       (false, ([], (Line "pbc", p)::pV_lists, 
                            (Line "pbc", p)::pV_lists, [Line "p"])),
                       tests)
val tests       = add ("Specific pattern - Oel, valid", 
                       patternValidation( (SOME r, Oel, [f_or,b_pir,b_qir]), 
                            [],pV_context1, true, "or_el", log),
                       (true, ([], (Line "or_el", r)::pV_lists, 
                            (Line "or_el", r)::pV_lists, [Line "p"])),
                       tests)
val tests       = add ("Specific pattern - Oel, not OR", 
                       patternValidation( (SOME p, Oel, [p, b_pir, b_qir]), 
                            [],pV_context1, true, "or_el", log),
                       (false, ([], (Line "or_el", p)::pV_lists, 
                            (Line "or_el", p)::pV_lists, [Line "p"])),
                       tests)
val tests       = add ("Specific pattern - Oel, wrong conclusion", 
                       patternValidation( (SOME p, Oel, [f_or,b_pir,b_qir]), 
                            [],pV_context1, true, "or_el", log),
                       (false, ([], (Line "or_el", p)::pV_lists, 
                            (Line "or_el", p)::pV_lists, [Line "p"])),
                       tests)
val tests       = add ("Specific pattern - Oel, mismatched referents", 
                       patternValidation( (SOME p, Oel, [f_or,b_qir,b_pir]), 
                            [],pV_context1, true, "or_el", log),
                       (false, ([], (Line "or_el", p)::pV_lists, 
                            (Line "or_el", p)::pV_lists, [Line "p"])),
                       tests)
(* Box validation tests *)
val tests       = add ("Box validation - valid",
                        boxValidation(pV_context1, ("p", p), 
                            true, "box", log),
                        (true, ([], pV_dis_box::pV_lists, [pV_dis_box],[])), 
                        tests)
val tests       = add ("Box validation - no assumptions",
                        boxValidation(pV_context0, ("p", p), 
                            true, "box", log),
                        (false,(pV_context0)), 
                        tests)
val tests       = add ("Box validation - assumption just made",
                        boxValidation(pV_context2, ("not_r", n_r), 
                            true, "box", log),
                        (false,([], pV_n_r_box::pV_lists, 
                                    pV_n_r_box::(tl pV_lists), [Line "p"])), 
                        tests)
val tests       = add ("Box validation - assumption not most recent",
                        boxValidation(pV_context2, ("p", p), 
                            true, "box", log),
                        (false,([], pV_dis_box::pV_lists, [pV_dis_box], [])), 
                        tests)
val tests       = add ("Box validation - assumption not listed",
                        boxValidation(pV_context1, ("not_r", n_r), 
                            true, "box", log),
                        (false,([], pV_n_r_box::pV_lists, 
                                    pV_n_r_box::pV_lists, [Line "p"])), 
                        tests)
(* ID validation *)
val tests       = add ("ID validation - Unique ID", 
                        idValidation("unique", pV_lists, log),
                       true, tests)
val tests       = add ("ID validation - Existing ID",
                        idValidation("p", pV_lists, log),
                       false, tests)
(* Proof validation *)
val tests       = add ("Proof validation - Valid", 
                        proofValidation(proof_val),
                       (true, "validation_unittest_valid.txt"), tests)
val tests       = add ("Proof validation - Invalid",
                        proofValidation(proof_inv),
                       (false, "validation_unittest_invalid.txt"), tests)

val return      = case (Mosml.argv ()) 
                    of [_,"full"] => "\n" | _ => "\r"
in
    val test = (TextIO.closeOut(log); results (rev tests, 0, 0, return))
end;
end;
