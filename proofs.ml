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

(* fn: formula list -> string *)
fun premisesToString []             = ""
  | premisesToString [f1]           =  formulaToString f1
  | premisesToString [f1, f2]       = (formulaToString f1)^" and "^
                                      (formulaToString f2)
  | premisesToString [f1, f2, f3]   = (formulaToString f1)^", "^
                                      (formulaToString f2)^", and "^
                                      (formulaToString f3)
  | premisesToString (f1::refs)     = (formulaToString f1)^", "^
                                      (premisesToString refs)

fun sequentToString (Sequent ([], f))       =
        "We wish to prove "^(formulaToString f)^"."
  | sequentToString (Sequent (forms, f))  =
        let val number = if (length forms = 1) then " " else "s "
        in
        "From the premise"^number^(premisesToString forms)^
        ", we wish to prove "^(formulaToString f)^"."
        end;

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
(* Patternmatching only gurantees acceptance of valid proofs, since it is 
 * only to be used with validated BoxProofs *)
fun proofstepsToString ([], _) = ""
  | proofstepsToString (Step(NONE, Dis, [ass], "")::steps, ind as t::ts)    =
        "\n"^(implode ts)^"Discharge assumption "^(refsToString [ass])^"."
        ^(proofstepsToString (steps, ts))
  | proofstepsToString (Step(SOME con, Ass, [], self)::steps, tabs)         =
        "\n"^(implode (tabs))^"Assume "^(formulaToString con)^" "^
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
        let val (wording, post) = if (self = "") 
                                 then (" we conclude ", ".")
                                 else (" we get ", " "^self^".")
        in
            "\n"^(implode tabs)^"By applying "^(ruleToString rule)^" to "^
            (refsToString refs)^wording^(formulaToString con)^post
            ^(proofstepsToString (steps, tabs))
        end
  | proofstepsToString _ = raise Match; (* Declare specific exception? *)

(* fn: proof -> string *)
fun proofToString (Proof (title, sequent, proofsteplist))  =
   "\n"^title^":\n"^(sequentToString sequent)^
   (proofstepsToString (proofsteplist, []));

(* fn bool*outstream*string -> bool *)
(* Returns given booliean, with the side-effect of outputting the given 
 * string to the given outstream when the boolean is false *)
fun feedback (true, _, _)           = true
  | feedback (false, out, message)  = TextIO.output(out, message) <> ()

(* Validation of the uniqueness of a given reference id *)
fun idValidation (self, allRefs, out) =
    let val comment = self^": This reference id has already been used."
        val valid   = 
        (List.find (fn (Line x, _) => (x = self) | (Box _, _) => false) 
        allRefs) = NONE
    in feedback (valid, out, comment) end;

(* Returns the proper line prefix based on validity of the proofstep *)
fun getPrefix(true, self) = self^": "
  | getPrefix(false, _)   = " ";

(* fn: 'a * 'a list -> bool *)
fun member (element, set) = List.exists (fn e => (element = e)) set

(* Validation of the basic pattern *)
fun rulePattern (rule, refs, prefix, out) =
    let val post = " when using "^(ruleToString rule)^"."
        val zero = (member (rule, [Lem, Ass, Prm])) andalso 
                   (feedback (refs = [], out, "You must not provide any "^
                    "references"^post)
                   )
        val one  = (member (rule, [Ae1, Ae2, Oi1, Oi2, Din, Del, Bel])) 
                   andalso 
                   (feedback (case refs of [Line _] => true | _ => false,
                    out, "You must provide exactly one reference to a "^
                    "single line"^post)
                   )
        val two  = (member (rule, [Mod, Ain, Iel, Nel])) andalso
                   (feedback (case refs of [Line _, Line _] => true 
                                         | _                => false, out,
                    "You must provide exactly two references to a single "^
                    "line"^post)
                   )
        val box  = (member (rule, [Iin, Nin, Pbc])) andalso
                   (feedback (case refs of [Box _] => true | _ => false, out,
                    "You must provide exactly one reference to a range of "^
                    "lines from an assumption to its discharging"^post)
                   )
        val oin  = (rule = Oel) andalso 
                   (feedback (case refs of [Line _, Box _, Box _] => true
                                         | _                      => false,
                    out, "You must provide exactly one reference to a "^
                    "single line and two references to ranges from "^
                    "assumptions to their discharging"^post)
                   )
    in
        (zero orelse one orelse two orelse box orelse oin)
    end;

(* Finds forms in reference table *)
fun findForm (reference:reference, table) = 
    List.mapPartial (fn (r, form:formula) => 
                    if (r = reference) then SOME form 
                                       else NONE) table;

(* fn: reference list * (reference*formula) list * (reference*formula) list
 *     * string * outstream -> bool * formula list *)
(* Validates references and returns forms using the two reference tables *)
fun matchRefs ([], _,_,_,_)                             = (true, [])
  | matchRefs (r::rs, allRefs, openRefs, prefix, out)   =
    let val formO          = findForm (r, openRefs)
        val formA          = findForm (r, allRefs) 
        val start          = prefix^"The reference "^(referenceToString r)
        val (valid, form)  = case (formO, formA) of
                 ([f], _)  => (true, SOME f)
               | ([], [])  => (feedback (false, out, start^
                               " does not exist."), NONE)
               | ([], [f]) => (feedback (false, out, start^" occurs under "^
                               "an assumption that has been discharged, "^
                               "thus the reference is not avialable at "^
                               "this point."), SOME f)
               | ([], _)   => (false, NONE)
               | (fl, _)   => (feedback (false, out, start^"is not unique; "^
                               "in the following, the last occurance has "^
                               "been used."),SOME (List.last fl))
        val prefix         = if valid then prefix else " "
        val (rest, flist)  = matchRefs(rs, allRefs, openRefs, prefix, out)
    in
        (valid andalso rest, (case form of NONE => flist 
                                         | SOME f => f::flist))
    end;

(* Returns the proper wording for mismatched pattersn *)
fun mismatch(rule, prefix, out, argument) =
    feedback (false, out, prefix^
              "You are using "^ruleToString(rule)^", but "^argument);

(* Remove closed references from the list of open references *)
fun closeBox(ass, []) = raise Match (* Should never occur due to earlier
                                          checks *)
  | closeBox(ass, (r, f)::refs) = if (r = ass) 
                                  then refs 
                                  else closeBox(ass, refs);

(* fn: (formula list*(reference*formula) list *(reference*formula) list
 *      reference list)*(string*formula)*bool*string*outstream ->
 *  -> (bool*(formula list*(reference*formula) list *(reference*formula) list
 *      reference list)) *)
(* Validate closure of box *)
fun boxValidation (context as (_, _, _, []), _, valid, self, out) =
        (feedback (false, out, (getPrefix(valid, self))^"You are trying "^
         "to discharge an assumption, but no assumptions are currently in "^
         "effect."), context)
  | boxValidation ((p, all, opn as (last, lForm)::rs, a::asums),
                   (aRef, aForm), valid, self, out) =
    let val ass   = Line aRef
        val valid = (feedback (ass <> last, out, 
                     (getPrefix(valid, self))^"Discharging an assumption "^
                     "immediately after making it is redundant.")) andalso
                     valid
        val valid = (feedback (a = ass, out, (getPrefix(valid, self))^
                      "The assumption you are trying to discharge, "^
                      "is not the one most recently made.")) andalso valid
        val (l, valid)  = case last of Line s  => (s, valid)
                                 | Box (_,  s) => (s, feedback (false, out, 
                                 (getPrefix(valid, self)^
                                 "You cannot discharge an assumption "^
                                 "immediately after another discharge.")))
        val line        = (Box (aRef, l), IMP(aForm, lForm))
        val opn         = closeBox(ass, opn)
    in
        (valid, (p, line::all, line::opn, asums))
    end
  | boxValidation ((_,_,[],_),_,_,_,_)     = raise Match 
    (* This should never occur if the asumption list is not empty *)

(* Validation of reference pattern *)
fun patternValidation (
    (SOME f, Ass, []), [],(prms, all, opn, asums), valid, self, out) =
    let val line = Line self
    in
        (true, (prms, (line, f)::all, (line, f)::opn, line::asums))
    end
  | patternValidation (
    (NONE, Dis, [f]), [Line aRef], context, valid, self, out) =
          boxValidation(context, (aRef, f), valid, self, out)
  | patternValidation (
    (SOME formula, rule, fList),_, context, valid, self, out) = 
    let val prefix                  = getPrefix(valid, self)
        val line                    = (Line self, formula)
        val bot                     = formulaToString BOT
        val (prms, all, opn, asums) = context
        val valid = case (rule, fList) of
            (Prm, [])       => feedback (member (formula, prms), out, prefix^
            "This formula is not listed as a premise in the sequent.")
          | (Cpy, [f])          => feedback (formula = f, out, prefix^
                                   "The line you are copying does not "^
                                   "refer to the formula you have stated.")
          | (Ain, [f1, f2])     => (case formula of
              AND(phi, psi)     => 
                let val first   = feedback (phi = f1, out, prefix^"The "^
                        "first reference does not match the first conjunct.")
                    val prefix  = getPrefix(valid andalso first, self)
                    val second  = feedback (psi = f2, out, prefix^"The "^
                        "second reference does not match the second "^
                        "conjunct.")
                in
                    first andalso second
                end
            | _                 => mismatch (rule, prefix, out, 
                                   "you do not introduce a conjunction.")
            )
          | (Ae1, [f])          => (case f of
              AND(phi, psi)     => feedback (formula = phi, out, prefix^
                                   "This line does not match the first "^
                                   "conjunct of the reference.")
            | _                 => mismatch (rule, prefix, out,
                                   "you do not refer to a conjunction.")
            )
          | (Ae2, [f])          => (case f of
              AND(phi, psi)     => feedback (formula = psi, out, prefix^
                                   "This line does not match the second "^
                                   "conjunct of the reference.")
            | _                 => mismatch (rule, prefix, out,
                                   "you do not refer to a conjunction.")
            )
          | (Oi1, [f])          => (case formula of
              OR(phi, psi)      => feedback (f = phi, out, prefix^
                                   "The referent does not match the first "^
                                   "disjunct in this line.")
            | _                 => mismatch (rule, prefix, out,
                                   "You do not introduce a disjunction.")
            )
          | (Oi2, [f])          => (case formula of
              OR(phi, psi)      => feedback (f = psi, out, prefix^
                                   "The referent does not match the second "^
                                   "disjunct in this line.")
            | _                 => mismatch (rule, prefix, out,
                                   "You do not introduce a disjunction.")
            )
          | (Iin, [f])          => (case formula of
              IMP _             => feedback (f = formula, out, prefix^
                                   "The referenced range does not match "^
                                   "this line.")
            | _                 => mismatch (rule, prefix, out,
                                   "you do not introduce an implication.")
            ) 
          | (Iel, [f1, f2])     => (case f2 of 
              IMP(phi, psi)       => 
                let val pre     = prefix^"Th"
                    val asum    = feedback (f1 = phi, out, pre^
                                   "e first referent does not match "^
                                   "the assumption of the second referent")
                    val pre     = if asum then pre else "; th"
                    val imp     = feedback (psi = formula, out, pre^
                                  "is line does not match the conclusion "^
                                  "of the second referent")
                in
                    feedback (asum andalso imp, out, ".")
                end
            | _                 => mismatch (rule, prefix, out,"the second "^
                                   "referent is not an implication.")
            ) 
          | (Nel, [f1, f2])     => if formula = BOT
            then feedback (f2 = (NEG f1), out, prefix^"The second "^
                 "referent is not the negation of the first referent.")
            else mismatch (rule, prefix, out,"you do not introduce "^bot^".")
          | (Din, [f])          => (case formula of
              NEG(NEG phi)      => feedback (f = phi, out, prefix^
                                   "This line is not is not the double-"^
                                   "negation of the referent.")
            | _                 => mismatch (rule, prefix, out,
                                   "you do not introduce a double-negation.")
            )
          | (Del, [f])          => (case f of
              NEG(NEG phi)      => feedback (formula = phi, out, prefix^
                                   "The referent is not is not the double-"^
                                   "negation of this line.")
            | _                 => mismatch (rule, prefix, out,
                                   "you do not refer to a double-negation.")
            )
          | (Bel, [f])          => (f = BOT) orelse 
                                   mismatch (rule, prefix, out,
                                   "you do not refer to "^bot^".")
          | (Mod, [f1, f2])     => (case f1 of
              IMP(phi, psi)     => 
                let val pre     = prefix^"Th"
                    val right   = feedback (f2 = (NEG phi), out, pre^
                                   "e second referent is not the negation "^
                                   "of the right-hand side of the "^
                                   "implication")
                    val pre     = if right then pre else "; th"
                    val left    = feedback (psi = formula, out, pre^
                                  "is line is not the negation of the "^
                                  "left-hand side of the implication")
                in
                    feedback (right andalso left, out, ".")
                end
            | _                 => mismatch(rule, prefix, out, "the first "^
                                   "referent is not an implication.")
            )
          | (Lem, [])           => (case formula of
              OR(phi, psi)      => feedback (psi = (NEG phi), out, prefix^
                                   "The second disjunct of this line is "^
                                   "not the negation of the first.")
            | _                 => mismatch (rule, prefix, out,
                                   "you do not introduce a conjunction.")
          )
          (* IMPs below are ensured by earlier checks *)
          | (Oel, [f, IMP(a1, x1), IMP(a2, x2)]) => (case f of
              OR(f1, f2)        => 
                let val pre     = prefix^"Th"
                    val mid     = "; th"
                    val first   = feedback (f1 = a1, out, pre^
                                  "e first disjunct of the first "^
                                  "referent does not match the assumption "^
                                  "of the second referent")
                    val pre     = if first then pre else mid
                    val second  = feedback (f2 = a2, out, pre^
                                  "e second disjunct of the first referent "^
                                  "does not match the assumption of the "^
                                  "third referent")
                    val pre     = if first andalso second then pre else mid
                    val matches = feedback (x1 = x2, out, pre^
                                  "e second and third referent do not "^
                                  "reach the same conclusion")
                    val final   = matches andalso (feedback (formula = x1,
                                  out, pre^"is line does not match the "^
                                  "conclusion reached by the second and "^
                                  "third referent"))
                in
                    feedback (first andalso second andalso final, out, ".")
                end
            | _                 => mismatch (rule, prefix, out, "the "^
                                   "first referent is not a disjunction.")
            )
          | (Nin, [IMP(f1,f2)]) => (case formula of
              NEG phi           => 
                let val pre     = prefix^"Th"
                    val asum    = feedback (f1 = phi, out, pre^
                                   "e assumption of the referent does not "^
                                   "match the negated formula of this line")
                    val pre     = if asum then pre else "; th"
                    val imp     = feedback (f2 = BOT, out, pre^
                                  "e conclusion of the referent is not "^bot)
                in
                    feedback (asum andalso imp, out, ".")
                end
            | _                 => mismatch (rule, prefix, out,
                                   "you do not introduce a negation.")
            ) 
          | (Pbc, [IMP(f1,f2)]) => if f2 = BOT
            then feedback (f1 = (NEG formula), out, prefix^
                 "The assumption of the referent is not the negation of "^
                 "this line.")
            else mismatch (rule, prefix, out, "the referenced range does "^
                 "not conlude "^bot)
          | _                   => raise Match (* should never happen *)

    in
      (valid, (prms, line::all, line::opn, asums))
    end
  | patternValidation ((NONE,_,_), _, _, _, _, _) = 
        raise Match (* should never happen *)

(* Validate rule and reference use *)
fun ruleValidation(valid, context, Step (prop, rule, refs, self), out) =
    let val (premises, allRefs, openRefs, asums) = context
        val prefix              = getPrefix(valid, self)
        val validPattern        = rulePattern(rule, refs, prefix, out)
        val prefix              = getPrefix(valid andalso validPattern, self)
        val (available, forms)  = matchRefs(refs, allRefs, openRefs, prefix,
                                            out)
        val prefix              = getPrefix(available, self)
        val pattern             = (prop, rule, forms)
        val (validUse, context) = 
            if validPattern andalso ((length forms) = (length refs))
            then patternValidation(pattern, refs, context, valid, self, out)
            else (false, context)
    in
        (valid andalso validUse andalso available, context)
    end;

(* Validation of a single step in the current context *)
fun stepValidation(valid, context, out, []) =
    let val error = "You cannot have a proof without any arguments."
    in
        (feedback(false, out, error), NONE, context)
    end
  | stepValidation(valid, context as (_, allRefs, _, _), out, step::steps)  =
    let val Step (prop, rule, refs, self)  = step
        val  stepValid                     = idValidation(self, allRefs, out)
        val (stepValid, context)           = ruleValidation(
             stepValid, context, step, out)
        val valid = (valid andalso stepValid)
    in
        if steps = [] 
        then (valid, prop, context)
        else stepValidation(valid, context, out, steps)
    end;

fun toBoxProof(Proof (title, seq, steplist)) = 
    "Here goes the BoxProover proof - to be implemented";
            
(* Validation of an entire proof *) 
fun proofValidation (proof as Proof (title, seq, steplist))  =
    let val filename            = "validation_"^title^".txt" (* Add checks *)
        val out                 = TextIO.openOut(filename)
        val Sequent (premises, goal) = seq
        val (valid, last, (_,_,_, asums)) = 
            stepValidation(true, (premises, [], [], []), out, steplist)
        val allDischarged       = 
            feedback (asums=[], out,
                      "Proof: Not all assumptions are discharged.")
        val prefix  = getPrefix(allDischarged, "Proof")
        val valid   = (case last of SOME c => feedback (c = goal, out,
                       prefix^"The conclusion does not match the goal.")
                                  | NONE   => feedback (false, out,
                       prefix^"The last line draws no conclusion.")) 
                      andalso valid andalso allDischarged
        val _ = feedback (not valid, out, toBoxProof(proof))
    in
        TextIO.closeOut(out)
    end;
