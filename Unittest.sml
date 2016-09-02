structure Unittest = struct
open Auxiliaries;

(* Validation of the uniqueness of a given reference id *)
fun idValidation (self, allRefs, out) =
    let val comment = 
        "\n["^self^"]: This reference id has already been used."
        val valid   = 
        (List.find (fn (Line x, _) => (x = self) | (Box _, _) => false) 
        allRefs) = NONE
    in feedback (valid, out, comment) end;

(* Validation of the basic pattern *)
fun rulePattern (Prm,  refs, prefix, out) =
    feedback(refs = [], out, prefix^"You cannot provide references when "^
                                    "stating a premise.")
  | rulePattern (Ass,  refs, prefix, out) =
    feedback(refs = [], out, prefix^"You cannot provide references when "^
                                    "stating an assumption.")
  | rulePattern (Cpy,  refs, prefix, out) =
    feedback(case refs of [Line _] => true | _ => false, out, prefix^
        "When copying, you must refer to one single line.")
  | rulePattern (Dis,  refs, prefix, out) =
    feedback(case refs of [Line _] => true | _ => false, out, prefix^
        "When discharging an assumption, you must refer to one single line.")
  | rulePattern (rule, refs, prefix, out) =
    let val post = " when using "^(ruleToString rule)^"."
        val zero = (rule = Lem) andalso (feedback (refs = [], out, prefix^
                   "You cannot provide references"^post)
                   )
        val one  = (member (rule, [Ae1, Ae2, Oi1, Oi2, Din, Del, Bel])) 
                   andalso 
                   (feedback (case refs of [Line _] => true | _ => false,
                    out, prefix^"You must provide exactly one reference "^
                    "to a single line"^post)
                   )
        val two  = (member (rule, [Mod, Ain, Iel, Nel])) andalso
                   (feedback (case refs of [Line _, Line _] => true 
                                         | _                => false, out,
                    prefix^"You must provide exactly two references to a "^
                    "single line"^post)
                   )
        val box  = (member (rule, [Iin, Nin, Pbc])) andalso
                   (feedback (case refs of [Box _] => true | _ => false, out,
                    prefix^"You must provide exactly one reference to a "^
                    "range of lines from an assumption to its discharging"^
                    post)
                   )
        val oel  = (rule = Oel) andalso 
                   (feedback (case refs of [Line _, Box _, Box _] => true
                                         | _                      => false,
                    out, prefix^"You must provide exactly one reference "^
                    "to a single line and two references to ranges from "^
                    "assumptions to their discharging"^post)
                   )
    in
        (zero orelse one orelse two orelse box orelse oel)
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
(*  | matchRefs ((Box (a,c))::rs, allRefs, openRefs, prefix, out)   =
*)
  | matchRefs (r::rs, allRefs, openRefs, prefix, out)   =
    let val formO          = findForm (r, openRefs)
        val formA          = findForm (r, allRefs) 
        val start          = prefix^"The reference "^(referenceToString r)
        val closed         = " occurs under or is itself "^
                             "an assumption that has been discharged, "^
                             "thus the reference is not avialable at "^
                             "this point"
        val repeated       = " is not unique"
        val (valid, form)  = case (formO, formA) of
                 ([f], _)  => (true, SOME f)
               | ([], [])  => let val msg =
                                (case r of 
                                  Line _ => start^" does not exist."
                                | Box  (a,c) => prefix^"Either ["^a^
                                "] does not match a properly discharged "^
                                "assumption or ["^c^"] does not match "^
                                "its conclusion (or both)."
                                )
                              in
                    (feedback (false, out, msg), NONE)
                              end
               | ([], [f]) => (feedback (false, out, start^closed^"."),
                                SOME f)
               | ([], fl)   => (feedback (false, out, start^repeated^
                                ", and every instance "),
                                NONE)
               | (fl, _)   => (feedback (false, out, start^repeated^
                               "; the last occurrence has been used."),
                               SOME (hd fl))
        val prefix         = if valid then prefix else " "
        val (rest, flist)  = matchRefs(rs, allRefs, openRefs, prefix, out)
    in
        (valid andalso rest, (case form of NONE => flist 
                                         | SOME f => f::flist))
    end;

(* Remove closed references from the list of open references *)
fun closeBoxOpen(ass, [])             = raise Match 
    (* Should never occur due to earlier checks *)
  | closeBoxOpen(ass, (r, f)::refs)   = if (r = ass) 
                                        then refs 
                                        else closeBoxOpen(ass, refs);

(* fn: reference * reference list -> reference list *)
(* Remove the given assumption and all the assumptions made after it *)
fun closeBoxAssums(ass, [])       = raise Match
    (* Should never occur due to earlier checks *)
  | closeBoxAssums(ass, a::asums) = if (a = ass) 
                                    then asums 
                                    else closeBoxAssums(ass, asums);

(* fn: (formula list*(reference*formula) list *(reference*formula) list
 *      reference list)*(string*formula)*bool*string*outstream ->
 *  -> (bool*(formula list*(reference*formula) list *(reference*formula) list
 *      reference list)) *)
(* Validate closure of box *)
fun boxValidation (context as (_, _, _, []), _, valid, self, out) =
        (feedback (false, out, (getPrefix(valid, self))^"You are trying "^
         "to discharge an assumption, but no assumptions are currently in "^
         "effect."), context)
  | boxValidation ((p, all, opn as (last, lForm)::rs, asums as a::rest),
                   (aRef, aForm), valid, self, out) =
    let val ass         = Line aRef  
        val valid       = (feedback (ass <> last, out, (getPrefix(valid, 
                           self))^"Discharging an assumption immediately "^
                           "after making it is redundant.")) andalso valid
        val isListed    = feedback (member (ass, asums), out,
                          (getPrefix(valid, self))^"Either the referent is "^
                          "not an assumption or it has already been "^
                          "discharged.")
        val valid       = isListed andalso
                          (feedback (a = ass, out, (getPrefix(valid, self))^
                           "The assumption you are trying to discharge, "^
                           "is not the most recently made "^
                           " of the undischarged assumptions.")) andalso 
                           valid
        val (l, valid)  = case last of Line s  => (s, valid)
                                 | Box (_,  s) => (s, feedback (false, out, 
                                 (getPrefix(valid, self)^
                                 "You cannot discharge an assumption "^
                                 "immediately after another discharge.")))
        val line        = (Box (aRef, l), IMP(aForm, lForm))
        val (opn,asums) = if isListed
                          then (closeBoxOpen(ass, opn),
                                closeBoxAssums(ass, asums))
                          else (opn, asums)
    in
        (valid, (p, line::all, line::opn, asums))
    end
  | boxValidation ((_,_,[],_),_,_,_,_)     = raise Match 
    (* This should never occur if the asumption list is not empty *)

(* Validation of reference pattern *)
fun patternValidation (
    (SOME f, Prm, []), [],(prms, all, opn, asums), valid, self, out) =
    let val line    = (Line self, f)
        val prefix  = getPrefix(valid, self)
        val valid   = 
            case prms of
                []          => feedback(false, out, prefix^
                "You cannot state a premise when all (if any) premises "^
                "in the sequent have already been stated.")
              | first::rest => feedback(f = first, out, prefix^
                "The premise you refer to is not the next in line.")
        val (p, ps) = List.partition (fn form => (form = f)) (prms)
    in
        (valid, (ps, line::all, line::opn, asums))
    end    
  | patternValidation (
    (SOME f, Ass, []), [],(prms,  all, opn, asums), valid, self, out) =
    let val line = Line self
    in
        (true, (prms, (line, f)::all, (line, f)::opn, line::asums))
    end
  | patternValidation (
    (NONE, Dis, [f]), [Line aRef], context, valid, self, out) =
          boxValidation(context, (aRef, f), valid, "Discharge", out)
  | patternValidation (
    (SOME formula, rule, fList),_, context, valid, self, out) = 
    let val prefix                  = 
        getPrefix(valid, self)^"You are using "^ruleToString(rule)^", but "
        val line                    = (Line self, formula)
        val bot                     = "a contradiction"
        val (prms, all, opn, asums) = context
        val valid = case (rule, fList) of
            (Cpy, [f])          => feedback (formula = f, out, prefix^
                                   "the line you are copying does not "^
                                   "refer to the formula you have stated.")
          | (Ain, [f1, f2])     => (case formula of
              AND(phi, psi)     => 
                let val first   = feedback (phi = f1, out, prefix^"the "^
                        "first referent does not match the first conjunct")
                    val prefix  = if first then prefix else "; "
                    val second  = feedback (psi = f2, out, prefix^"the "^
                        "second referent does not match the second "^
                        "conjunct")
                in
                    feedback (first andalso second, out, ".")
                end
            | _                 => feedback (false, out, prefix^ 
                                   "you do not introduce a conjunction.")
            )
          | (Ae1, [f])          => (case f of
              AND(phi, psi)     => feedback (formula = phi, out, prefix^
                                   "this line does not match the first "^
                                   "conjunct of the referent.")
            | _                 => feedback (false, out, prefix^
                                   "you do not refer to a conjunction.")
            )
          | (Ae2, [f])          => (case f of
              AND(phi, psi)     => feedback (formula = psi, out, prefix^
                                   "this line does not match the second "^
                                   "conjunct of the referent.")
            | _                 => feedback (false, out, prefix^
                                   "you do not refer to a conjunction.")
            )
          | (Oi1, [f])          => (case formula of
              OR(phi, psi)      => feedback (f = phi, out, prefix^
                                   "the referent does not match the first "^
                                   "disjunct in this line.")
            | _                 => feedback (false, out, prefix^
                                   "You do not introduce a disjunction.")
            )
          | (Oi2, [f])          => (case formula of
              OR(phi, psi)      => feedback (f = psi, out, prefix^
                                   "the referent does not match the second "^
                                   "disjunct in this line.")
            | _                 => feedback (false, out, prefix^
                                   "You do not introduce a disjunction.")
            )
          | (Iel, [f1, f2])     => (case f2 of 
              IMP(phi, psi)       => 
                let val asum    = feedback (f1 = phi, out, prefix^
                                   "the first referent does not match "^
                                   "the antecedent of the second referent")
                    val prefix  = if asum then prefix else "; "
                    val imp     = feedback (psi = formula, out, prefix^
                                  "this line does not match the consequent "^
                                  "of the second referent")
                in
                    feedback (asum andalso imp, out, ".")
                end
            | _                 => feedback (false, out, prefix^"the second "^
                                   "referent is not an implication.")
            ) 
          | (Nel, [f1, f2])     => if formula = BOT
            then feedback (f2 = (NEG f1), out, prefix^"the second "^
                 "referent is not the negation of the first referent.")
            else feedback (false, out, prefix^"you do not introduce "^bot^".")
          | (Din, [f])          => (case formula of
              NEG(NEG phi)      => feedback (f = phi, out, prefix^
                                   "the formula which this line double-"^
                                   "negates does not match the referent.")
            | _                 => feedback (false, out, prefix^
                                   "you do not introduce a double-negation.")
            )
          | (Del, [f])          => (case f of
              NEG(NEG phi)      => feedback (formula = phi, out, prefix^
                                   "the referent is not is not the double-"^
                                   "negation of this line.")
            | _                 => feedback (false, out, prefix^
                                   "you do not refer to a double-negation.")
            )
          | (Bel, [f])          => (f = BOT) orelse 
                                   feedback (false, out, prefix^
                                   "you do not refer to "^bot^".")
          | (Mod, [f1, f2])     => (case f1 of
              IMP(phi, psi)     => 
                let val right   = feedback (f2 = (NEG psi), out, prefix^
                                   "the second referent is not the negation "^
                                   "of the consequent of the implication")
                    val prefix  = if right then prefix else "; "
                    val left    = feedback (formula = NEG(phi), out, prefix^
                                  "this line is not the negation of the "^
                                  "antecedent of the implication")
                in
                    feedback (right andalso left, out, ".")
                end
            | _                 => feedback (false, out, prefix^"the first "^
                                   "referent is not an implication.")
            )
          | (Lem, [])           => (case formula of
              OR(phi, psi)      => feedback (psi = (NEG phi), out, prefix^
                                   "the second disjunct of this line is "^
                                   "not the negation of the first.")
            | _                 => feedback (false, out, prefix^
                                   "you do not introduce a conjunction.")
          )
          (* IMPs below are ensured by earlier checks *)
          | (Iin, [IMP(a, x)])  => (case formula of
              IMP(phi, psi)     => 
                let val asum    = feedback (a = phi, out, prefix^
                                  "the assumption of the referent does not "^
                                  "match the antecedent of the "^
                                  "implication being introduced")
                    val prefix  = if asum then prefix else "; "
                    val imp     = feedback (x = psi, out, prefix^
                                  "the conclusion of the referent does not "^
                                  "match the consequent of the "^
                                  "implication being introduces")
                in
                    feedback (asum andalso imp, out, ".")
                end
            | _                 => feedback (false, out, prefix^
                                   "you do not introduce an implication.")
            ) 
          | (Oel, [f, IMP(a1, x1), IMP(a2, x2)]) => (case f of
              OR(f1, f2)        => 
                let val mid     = "; "
                    val first   = feedback (f1 = a1, out, prefix^
                                  "the first disjunct of the first "^
                                  "referent does not match the assumption "^
                                  "of the second referent")
                    val prefix  = if first then prefix else mid
                    val second  = feedback (f2 = a2, out, prefix^
                                  "the second disjunct of the first referent "^
                                  "does not match the assumption of the "^
                                  "third referent")
                    val prefix  = if first andalso second then prefix 
                                                          else mid
                    val matches = feedback (x1 = x2, out, prefix^
                                  "the second and third referent do not "^
                                  "reach the same conclusion")
                    val final   = matches andalso (feedback (formula = x1,
                                  out, prefix^"this line does not match "^
                                  "the conclusion reached by the second "^
                                  "and third referent"))
                in
                    feedback (first andalso second andalso final, out, ".")
                end
            | _                 => feedback (false, out, prefix^ "the "^
                                   "first referent is not a disjunction.")
            )
          | (Nin, [IMP(f1,f2)]) => (case formula of
              NEG phi           => 
                let val asum    = feedback (f1 = phi, out, prefix^
                                   "the assumption of the referent does not "^
                                   "match the negated formula of this line")
                    val prefix  = if asum then prefix else "; "
                    val imp     = feedback (f2 = BOT, out, prefix^
                                  "the conclusion of the referent is not "^bot)
                in
                    feedback (asum andalso imp, out, ".")
                end
            | _                 => feedback (false, out, prefix^
                                   "you do not introduce a negation.")
            ) 
          | (Pbc, [IMP(f1,f2)]) => if f2 = BOT
            then feedback (f1 = (NEG formula), out, prefix^
                 "the assumption of the referent is not the negation of "^
                 "this line.")
            else feedback (false, out, prefix^ "the referenced range does "^
                 "not conlude "^bot)
          | _                   => raise Match (* should never happen *)

    in
      (valid, (prms, line::all, line::opn, asums))
    end
  | patternValidation ((NONE,_,_), _, _, _, _, _) = 
        raise Match (* should never happen *)

(* Validate rule and reference use *)
fun ruleValidation(valid, context, (prop, rule, refs, self), out) =
    let val (premises, allRefs, openRefs, asums) = context
        val self                = if rule = Dis then "Discharge" else self
        val prefix              = getPrefix(valid, self)
        val validPattern        = rulePattern(rule, refs, prefix, out)
        val prefix              = getPrefix(valid andalso validPattern, self)
        val (available, forms)  = matchRefs(refs, allRefs, openRefs, prefix,
                                            out)
        val prefix              = getPrefix(available, self)
        val pattern             = (prop, rule, forms)
        val false_context       = case prop of NONE => context
                                | SOME f => (premises, 
                                            (Line self, f)::allRefs, 
                                            (Line self, f)::openRefs, asums)
        val valid = valid  andalso available
        val (validUse, context) = 
            if validPattern andalso ((length forms) = (length refs))
            then patternValidation(pattern, refs, context, valid, self, out)
            else (false, false_context)
    in
        (valid andalso validUse, context)
    end;

(* Validation of a single step in the current context *)
fun stepValidation(valid, context, out, []) =
    let val error = "You cannot have a proof without any arguments."
    in
        (feedback(false, out, error), NONE, context)
    end
  | stepValidation(valid, context as (prms, all, _, _), out, step::steps)  =
    let val (prop, rule, refs, self)     = step
        val  lastStep           = (steps = [])
        val  id                 = case rule of Dis => "Discharge" | _ => self
        val  order              = (rule = Prm) orelse
            (feedback (prms = [], out, (getPrefix(true, id))^
             "You must list all premises before starting your deductions."))
        val  stepValid                = (self = "") orelse
                                             idValidation(self, all, out)
        val (stepValid, context)      = ruleValidation(order andalso
             stepValid, context, step, out)
        val valid = (valid andalso order andalso stepValid)
    in
        if lastStep then (valid, prop, context)
                    else stepValidation(valid, context, out, steps)
    end;

(* Validation of an entire proof *) 
fun proofValidation (proof as (title, seq, steplist))  =
    let val filename            = "validation_"^title^".txt"
        val out                 = TextIO.stdErr
        val (premises, goal) = seq
        val (valid, last, (_,_,_, asums)) = 
            stepValidation(true, (premises, [], [], []), out, steplist)
        val allDischarged       = 
            feedback (asums=[], out,
                      "\nProof: Not all assumptions are discharged.")
        val prefix  = getPrefix(allDischarged, "Proof")
        val valid   = (case last of SOME c => feedback (c = goal, out,
                       prefix^"The conclusion does not match the goal.")
                                  | NONE   => feedback (false, out,
                       prefix^"The last line draws no conclusion.")) 
                      andalso valid andalso allDischarged
        val _       = if valid 
                      then TextIO.output (TextIO.stdOut, toBoxProof(proof))
                      else TextIO.output (TextIO.stdErr, "\n")
    in
        valid
    end;
end
