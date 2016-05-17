use "datatypes.ml";

fun bracket(s) = "("^s^")";

fun formulaToString(Atom c)         = Char.toString c
  | formulaToString(BOT)            = "\226\138\165"
  | formulaToString(NEG f)          = "\194\172"^negToString(f)
  | formulaToString(AND (f1,f2))    = andToString(f1)^"\226\136\167"^
                                      negToString(f2)
  | formulaToString(OR (f1, f2))    = impToString(f1)^"\226\136\168"^
                                      andToString(f2)
  | formulaToString(IMP (f1, f2))   = impToString(f1)^"\226\134\146"^
                                      formulaToString(f2)

and negToString(a as Atom _)        = formulaToString(a)
  | negToString(n as NEG _)         = formulaToString(n)
  | negToString(f as _)             = bracket(formulaToString(f))

and andToString(f as OR _)          = bracket(formulaToString(f))
  | andToString(f as IMP _)         = bracket(formulaToString(f))
  | andToString(f as _)             = formulaToString(f)

and impToString(f as IMP _)         = bracket(formulaToString(f))
  | impToString(f as _)             = formulaToString(f);

fun printl s = print(s^"\n");

(* fun sqBracket s = "["^s^"]"; *)

fun referenceToString (Line s)      = s
  | referenceToString (Box (s1,s2)) = s1^"-"^s2;

(* fn: reference list -> string *)
fun refsToString []             = ""
  | refsToString [r1]           =  referenceToString r1
  | refsToString [r1, r2]       = (referenceToString r1)^" and "^
                                  (referenceToString r2)
  | refsToString [r1, r2, r3]   = (referenceToString r1)^", "^
                                  (referenceToString r2)^", and "^
                                  (referenceToString r3)
  | refsToString (r1::refs)     = (referenceToString r1)^", "^
                                  (refsToString refs)

fun premisesToString ([f])    = "and "^formulaToString f
  | premisesToString (f::fs)  = (formulaToString f)^", "^premisesToString fs

fun sequentToString (Sequent ([], f))       =
        "We wish to prove "^(formulaToString f)^"."
  | sequentToString (Sequent ([f1], f))  =
        "From the premise "^(formulaToString f1)^", we wish to prove "^
        (formulaToString f)^"."
  | sequentToString (Sequent ([f1,f2], f))  =
        "From the premises "^(formulaToString f1)^" and "^
        (formulaToString f2)^", we wish to prove "^(formulaToString f)^"."
  | sequentToString (Sequent (fs, f))       =
        "From the premises "^(premisesToString fs)^", we wish to prove "^
        (formulaToString f)^".";

fun ruleToString r =
    case r of Ain => "the and-introduction rule"
            | Ae1 => "the first and-elimination rule"
            | Ae2 => "the second and-elimination rule"
            | Oi1 => "the first or-introduction rule"
            | Oi2 => "the second or-introduction rule"
            | Oel => "the or-elimination rule"
            | Iin => "the implication-introduction rule"
            | Ie1 => "the implication-elimination rule"
            | Nin => "the negation-introduction rule"
            | Ne1 => "the negation-elimination rule"
            | Din => "introduction of double-negation"
            | De1 => "elimination of double-negation"
            | Be1 => "elimination of absurdity"
            | Mod => "Modus Tollens"
            | Pbc => "proof by contradiction"
            | Lem => "the law of the excluded middle"

(* fn: proofstep * char list -> string *)
(* Patternmatchin only gurantees acceptance of valid proofs, since it is 
 * only to be used with validated BoxProofs *)
fun proofstepsToString ([], _) = ""
  | proofstepsToString (Step(NONE, Dis, [ass], "")::steps, ind as t::ts)    =
        "\n"^(implode ind)^"Discharge assumption "^(refsToString [ass])^"."
        ^(proofstepsToString (steps, ts))
  | proofstepsToString (Step(SOME con, Ass, [], self)::steps, tabs)         =
        "\n"^(implode (#"\t"::tabs))^"Assume "^(formulaToString con)^" "^
        self^"."
        ^(proofstepsToString (steps, #"\t"::tabs))
  | proofstepsToString (Step(SOME con, Prm, [], self)::steps, tabs)         =
        "\n"^(implode tabs)^"We have "^(formulaToString con)^
        " as a premise "^self^"."
        ^(proofstepsToString (steps, tabs))
  | proofstepsToString (Step(SOME (c as OR _), Lem, [], self)::steps, tabs) =
        "\n"^(implode tabs)^"In accordance with "^(ruleToString Lem)^
        ", we introduce "^(formulaToString c)^" "^self^"."
        ^(proofstepsToString (steps, tabs))
  | proofstepsToString (Step(SOME con, Cpy, [Line org], self)::steps, tabs) =
        "\n"^(implode tabs)^"By copying "^org^", we get "^
        (formulaToString con)^" here too "^self^"."
        ^(proofstepsToString (steps, tabs))
  | proofstepsToString (Step(SOME con, rule, refs, self)::steps, tabs)      =
        let val wording = if (self = "") then " we conclude "
                                         else " we get "
        in
            "\n"^(implode tabs)^"By applying "^(ruleToString rule)^" to "^
            (refsToString refs)^wording^(formulaToString con)^" "^self^"."
            ^(proofstepsToString (steps, tabs))
        end;

fun proofToString (Proof (title, sequent, proofsteplist))  =
   "\n"^title^":\n"^(sequentToString sequent)^
   (proofstepsToString (proofsteplist, []));
(*
(* Validation of the uniqueness of a given reference id *)
fun idValidation (self, allRefs, out) =
    let val id      = case self of Line s => s | Box (ass, s) => s
        val comment = "["^id^"]: This reference id has already been used."
        val valid   = ((List.find (fn (x, _) => (x = id)) allRefs) = NONE)
                      orelse TextIO.output(out, comment) <> ()
    in valid end;

(* Returns the proper line prefix based on validity of the proofstep *)
fun getPrefix(true, self)  = 
    let val id          = case self of Line s => s | Box (a, s) => s
    in  "["^id^"]: " 
    end
  | getPrefix(false, _)    = " ";

(* Returns the correct number of references for a rule *)
fun ruleRefs (r as rule) =
    case r of Lem => 0
            | Oel => 3
            | Mod => 2
            | Ain => 2
            | Iel => 2
            | Nel => 2
            | _   => 1

(* Finds forms in reference table *)
fun findRefs (references, table) = 
            map (fn id => List.mapPartial (fn (r, form) => 
            if (r = id) then SOME form else NONE) table) references;

(* Returns the proper wording for mismatched pattersn *)
fun mismatch(rule, argument) =
    (false, "You are using "^ruleToString(rule)^", but "^argument);

(* Validation of reference pattern *)
fun patternValidation (pattern, out, prefix) = 
    let val (valid, comment)    =  case pattern of
        (prop, Cpy, [[form]])                                   => 
            (prop = form, "The line you are copying "^
            "does not refer to the formula you have stated.")
      | (AND(phi, psi), Ain, [[f1], [f2]])                      => 
            (phi = f1 andalso psi = f2,
            "The two references do not match the conjuncts in this line.")
      | (_, Ain, _)                                             => 
            mismatch(Ain, "do not introduce a conjunction.")
      | (prop, Ae1, [[AND(f1, f2)]])                            =>
            (prop = f1,
            "This line does not match the first conjunct of the reference.")
      | (prop, Ae1, [[_]])                                      => 
            mismatch(Ae1, "do not refer to a conjunction.")
      | (prop, Ae2, [[AND(f1,f2)]])                             =>
            (prop = f2,
            "This line does not match the second conjunct of the reference.")
      | (_, Ae2, [[_]])                                         =>
            mismatch(Ae2, "do not refer to a conjunction.")
      | (OR(phi, psi), Oi1, [[form]])                           =>
            (phi = form,
            "The reference does not match the first disjunct in this line.")
      | (_, Oi1, [[form]])                                      =>
            mismatch(Oi1, "do not introduce a disjunction.")
      | (OR(phi, psi), Oi2, [[form]])                           =>
            (psi = form,
            "The reference does not match the second disjunct in this line.")
      | (_, Oi2, [[form]])                                      =>
            mismatch(Oi2, "do not introduce a disjunction.")
      | (prop, Oel, [[OR(f1,f2)],[IMP(a1,x1)],[IMP(a2,x2)]])    =>
            let val v1 = (f1 = a1)
                val v2 = (f2 = a2)
                val v3 = (x1 = x2)
                val v4 = (x1 = prop)
                val (comment, prefix) = if v1 then ("","") 
                    else ("The first disjunct does not match the first "^
                     "assumption."," ")
                val (comment, prefix) = if v2 then (comment, prefix) 
                    else (comment^prefix^"The second disjunct does not "^
                     "match the second assumption.", " ")
                val (comment, prefix) = if v3 then (comment, prefix)
                    else ("The two assumptions do not lead to the same "^
                          "part-conclusion.", " ")
                val comment = if v3 andalso not v4 
                    then comment^"This line does not match the part-"^
                         "conclusion reached from the two assumptions."
                    else comment
            in
                (v1 andalso v2 andalso v3 andalso v4, comment)
            end
      | (_, Oel, [[_], [IMP _],[IMP _]])                        => 
            mismatch(Oel, "the first reference is not to a disjunction.")
      | (_, Oel, [[OR _], [_], [_]])                            =>
            mismatch(Oel, "the second and third references are not (both) "^
                          "to boxes.")
      | (IMP (phi,psi), Iin,[[IMP (a, c)]])                     =>
            let val v1                  = (phi = a)
                val v2                  = (psi = c)
                val box                 = " of the referenced box"
                val (comment, pre, box) = if v1 
                    then ("","This line does not ", box) 
                    else ("This line does not match the assumption"^box,
                          ", nor does it ", ".")
                val comment             = if v2 then comment^"."
                    else pre^"match the part-conclusion"^box
            in
                (v1 andalso v2, comment)
            end
      | (IMP _, Iin, [[_]])                                     =>
            mismatch(Iin, "do not refer to a box.")
      | (_, Iin, [[_]])                                         =>
            mismatch(Iin, "do not introduce an implication.")
(*      | (prop, Iel, [[f],[IMP(f1,f2)]])                         =>
      | (_, Iel, [[_],[_]])                                     =>
      | (prop, Nin, [[IMP(form, BOT)]])                         =>
      | (_, Nin, [[_]])                                         =>
      | (BOT, Nel, [[f1], [NEG(f2)]])                           =>
      | (BOT, Nel, [[_], [_]])                                  =>
      | (_,   Nel, [[_], [_]])                                  =>
      | (NEG(NEG(phi)), Din, [[form]])                          =>
      | (_, Din, [[_]])                                         =>
      | (prop, Del, [[NEG(NEG(f))]])                            =>
      | (_, Del, [[_]])                                         =>
      | (prop, Bel, [[BOT]])                                    =>
      | (_, Bel, [[_]])                                         =>
      | (NEG(phi), Mod, [[IMP(f1, f2)], [NEG(f3)]])             =>
      | (NEG(phi), Mod, [[IMP _], [_]])                         =>
      | (NEG(phi), Mod, [[_], [NEG _]])                         =>
      | (_, Mod, [[_],[_]])                                     =>
      | (prop, Pbc, [[IMP(NEG(f1), BOT)]])                      =>
      | (_, Pbc, [[IMP(NEG(f1), _)]])                           =>
      | (_, Pbc, [[IMP(_, _)]])                                 =>
      | (_, Pbc, [[_]])                                         =>
      | (OR(phi, NEG(psi)), Lem, [])                            =>*)
      | (_, _, _) => (true, "To be implemented.")
    in
        valid orelse TextIO.output(out, prefix^comment) <> ()
    end;

(* Validate rule and reference use *)
fun ruleValidation(valid, context, prop, rule, refs, out, self) =
    (*let val prefix  = getPrefix(valid, self)
        val (premises, allRefs, openRefs, asums) = context
        (* Checks the given number of references against requirement *)
        (* Replace with check of basic patterns *)
        val given   = length refs
        val correct = ruleRefs(rule)
        val valid   = (given = correct) orelse
        TextIO.output(out, prefix^"Using "^ruleToString(rule)^" requires "^
        (*Int.toString(correct)*)"right number"^" references, you gave "^
        (*Int.toString(given)*)"wrong number"^".") <> ()
        val prefix  = getPrefix(valid, self)
        (* Checks that the given references follow the proper pattern *)
        val forms   = findRefs(refs, allRefs)
        (* Insert check with openRefs here? *)
        val pattern = (prop, rule, forms)
        (* Temporary solution to asums as strings *)
        val id = case self of Line s => s | _ => "error"
        val (patternValid, asums) = case rule of 
            Prm => (List.exists (fn prm => (prm = prop)) premises
                   orelse TextIO.output(out, prefix^"This formula is not "^
                   "given as a premise.") <> (), asums)
          | Ass => (forms = [], id::asums)
          | _   => (patternValidation(pattern, out, prefix), asums)
    in
        (valid andalso patternValid, (premises, allRefs, openRefs, asums))
    end;*) (true, context)

(* Remove closed references from the list of open references *)
fun closeRefs(ass, []) = [] (* Should raise exception *)
  | closeRefs(ass, r::refs) = if (r = Line ass) then refs 
                                               else closeRefs(ass, refs);

(* Validate closure of box *)
fun boxValidation (valid, context as (p, a, openRefs, []), ass, out, self) =
    let val prefix  = getPrefix(valid, self)
        val valid   = (TextIO.output(out, prefix^"You are trying to "^
            "discharge an assumption, but no assumptions are currently "^
            "in effect.") <> ())
    in
        (valid, context)
    end
  | boxValidation (valid, (p, all, openRefs, a::asums), ass, out, self) =
    let val prefix      = getPrefix(valid, self)
        val discharged  = (a = ass) orelse TextIO.output(out,
            prefix^"The assumption you are trying to discharge, is not "^
            "the one most recently made.") <> ()
        val openRefs    =  if discharged then closeRefs(ass, openRefs)
                                         else openRefs
    in
        (valid andalso discharged, (p, all, openRefs, asums))
    end;

(* Validation of a single step in the current context *)
fun stepValidation(valid, context as (_,_,_, asums), out, []) =
    (* Add feedback for empty steplist *)
    (false, NONE, asums)
  | stepValidation(valid, context as (_, allRefs, _, _), out, step::steps)  =
    let val Step (prop, rule, refs, self)    = step
        val  stepValid                  = idValidation(self, allRefs, out)
        val (stepValid, context)        = ruleValidation(
             stepValid, context, prop, rule, refs, out, self)
        val (valid, context)            = case self of 
                Box(ass, _) => boxValidation(valid, context, ass, out, self)
              | Line s      => (valid, context)
        val valid = (valid andalso stepValid)
        val (_,_,_,asums)               = context (* move to proof? *)
    in
        case steps of [] => (valid, SOME prop, asums)
                    |  _ => stepValidation(valid, context, out, steps)
    end;

fun toBoxProof(Proof (title, seq, steplist)) = 
    "Here goes the BoxProover proof - to be implemented";
            
(* Validation of an entire proof *) 
fun proofValidation (proof as Proof (title, seq, steplist))  =
    let val filename            = "validation_"^title^".txt" (* Add checks *)
        val out                 = TextIO.openOut(filename)
        val Sequent (premises, goal) = seq
        val (valid, last, asms) = 
            stepValidation(true, (premises, [], [], []), out, steplist)
        val allDischarged       = (asms = []) orelse TextIO.output(out, 
            "Proof: Not all assumptions are discharged.") <> ()
        val prefix  = if allDischarged then "Proof: " else " "
        val valid   = case last of SOME c => (c = goal) andalso allDischarged
                                 | NONE   => TextIO.output(out,
                    prefix^"The conclusion does not match the goal.") <> ()
        val _ = if valid then TextIO.output(out, toBoxProof(proof)) else ()
    in
        TextIO.closeOut(out)
    end;
*)
