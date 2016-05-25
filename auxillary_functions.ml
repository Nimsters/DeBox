fun printl s = print(s^"\n");

fun bracket(s) = "("^s^")";

(* fn: formula -> string *)
fun formulaToString(Atom s)         = s
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

fun referenceToString (Line s)      = "["^s^"]"
  | referenceToString (Box (s1,s2)) = "["^s1^"]-["^s2^"]";

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

fun sequentToString ([], f)     =
        "We wish to prove "^(formulaToString f)^"."
  | sequentToString (forms, f)  =
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
  | proofstepsToString ((NONE, Dis, [ass], "")::steps, ind as t::ts)    =
        "\n"^(implode ts)^"Discharge assumption "^(refsToString [ass])^"."
        ^(proofstepsToString (steps, ts))
  | proofstepsToString ((SOME con, Ass, [], self)::steps, tabs)         =
        "\n"^(implode (tabs))^"Assume "^(formulaToString con)^" ["^self^"]."
        ^(proofstepsToString (steps, #"\t"::tabs))
  | proofstepsToString ((SOME con, Prm, [], self)::steps, tabs)         =
        "\n"^(implode tabs)^"We have the premise "^(formulaToString con)^
        " ["^self^"]."
        ^(proofstepsToString (steps, tabs))
  | proofstepsToString ((SOME (c as OR _), Lem, [], self)::steps, tabs) =
        "\n"^(implode tabs)^"In accordance with "^(ruleToString Lem)^
        ", we introduce "^(formulaToString c)^" ["^self^"]."
        ^(proofstepsToString (steps, tabs))
  | proofstepsToString ((SOME con, Cpy, [Line org], self)::steps, tabs) =
        "\n"^(implode tabs)^"By copying "^org^", we get "^
        (formulaToString con)^" here too ["^self^"]."
        ^(proofstepsToString (steps, tabs))
  | proofstepsToString ((SOME con, rule, refs, self)::steps, tabs)      =
        let val post = if (self = "") then "." else " ["^self^"]."
        in
            "\n"^(implode tabs)^"By applying "^(ruleToString rule)^" to "^
            (refsToString refs)^", we get "^(formulaToString con)^post
            ^(proofstepsToString (steps, tabs))
        end
  | proofstepsToString _ = raise Match; (* Declare specific exception? *)

(* fn: proof -> string *)
fun proofToString (title, sequent, proofsteplist)  =
   "\n"^title^":\n"^(sequentToString sequent)^
   (proofstepsToString (proofsteplist, []));

(* fn: formula -> string *)
fun formulaToBoxProof(Atom s)         = s
  | formulaToBoxProof(BOT)            = "bot"
  | formulaToBoxProof(NEG f)          = " ~ "^negToBoxProof(f)
  | formulaToBoxProof(AND (f1,f2))    = andToBoxProof(f1)^" /\\ "^
                                      negToBoxProof(f2)
  | formulaToBoxProof(OR (f1, f2))    = impToBoxProof(f1)^" \\/ "^
                                      andToBoxProof(f2)
  | formulaToBoxProof(IMP (f1, f2))   = impToBoxProof(f1)^" => "^
                                      formulaToBoxProof(f2)

and negToBoxProof(a as Atom _)        = formulaToBoxProof(a)
  | negToBoxProof(n as NEG _)         = formulaToBoxProof(n)
  | negToBoxProof(f as _)             = bracket(formulaToBoxProof(f))

and andToBoxProof(f as OR _)          = bracket(formulaToBoxProof(f))
  | andToBoxProof(f as IMP _)         = bracket(formulaToBoxProof(f))
  | andToBoxProof(f as _)             = formulaToBoxProof(f)

and impToBoxProof(f as IMP _)         = bracket(formulaToBoxProof(f))
  | impToBoxProof(f as _)             = formulaToBoxProof(f)
  ;

fun referenceToBoxProof (Line s)      = "@"^s
  | referenceToBoxProof (Box (a,c))   = "#"^a^"-"^c
  ;

(* fn: reference list -> string *)
fun refsToBoxProof []             = ""
  | refsToBoxProof (r1::refs)     = 
        (referenceToBoxProof r1)^" "^(refsToBoxProof refs)
  ;

(* fn: formula list -> string *)
fun premisesToBoxProof []             = ""
  | premisesToBoxProof (f1::refs)     = 
        (formulaToBoxProof f1)^" , "^(premisesToBoxProof refs)

fun sequentToBoxProof (forms, f)  =
        (premisesToBoxProof forms)^" |- "^(formulaToBoxProof f);

fun ruleToBoxProof r =
    case r of Ain => "con_i "
            | Ae1 => "con_e1 "
            | Ae2 => "con_e2 "
            | Oi1 => "dis_i1 "
            | Oi2 => "dis_i2 "
            | Oel => "dis_e "
            | Iin => "imp_i "
            | Ie1 => "imp_e "
            | Nin => "neg_i "
            | Ne1 => "neg_e "
            | Din => "nni "
            | De1 => "nne "
            | Be1 => "bot_e "
            | Mod => "mt "
            | Pbc => "pbc "
            | Lem => "lem "
            ;

(* fn: proofstep * char list -> string *)
(* Patternmatching only gurantees acceptance of valid proofs, since it is 
 * only to be used with validated proofs *)
fun proofstepsToBoxProof ([], _) = ""
  | proofstepsToBoxProof ((SOME con, rule, refs, self)::steps, ind) =
        let val prefix          = implode ind
            val (ind, start)    = if rule = Ass 
                                  then (#"\t"::ind, prefix^"( ") 
                                  else (ind, prefix)
            val (rest, indent, post)    = 
                case steps of (NONE, Dis, [Line a],_)::tail =>
                  let val ts    = case ind of t::ts => ts | ts => ts
                  in
                   (tail, ts, "\n"^(implode ts)^") ; [#"^a^"-"^self^"]")
                  end
                  | []    => ([], [], "\n.")
                  | tail  => (tail, ind, "; [@"^self^"]")
            val argument  = case rule of
                    Ass        => " assumption"
                  | Prm        => " premise"
                  | Dis        => raise Option (* Cannot happen *)
                  | _          => 
                    " by "^(ruleToBoxProof rule)^(refsToBoxProof refs)
        in
            "\n"^start^(formulaToBoxProof con)^argument^post
            ^(proofstepsToBoxProof (rest, indent))
        end
  | proofstepsToBoxProof ((_, Dis, _,_)::steps, _) = raise Match
  ;

fun wrap cs (s1, s2) = 
    foldr (fn (a,b) => a^b) "" (map (fn c => s1^c^s2) cs);

(* fn: 'a * 'a list -> bool *)
fun member (element, set) = List.exists (fn e => (element = e)) set

fun findAtoms [] l      = l 
  | findAtoms (f::fs) l = 
    let val here        = case f of
          Atom a        => if member(a, l) then l else a::l
        | NEG  n        => findAtoms [n] l
        | BOT           => l
        | AND (f1, f2)  => findAtoms [f1] (findAtoms [f2] l)
        | IMP (f1, f2)  => findAtoms [f1] (findAtoms [f2] l)
        | OR  (f1, f2)  => findAtoms [f1] (findAtoms [f2] l)
    in
        findAtoms fs here
    end
  ;

fun checkTitle title = ""^title

(* fn: proof -> string *)
fun toBoxProof (title, seq as (forms, form), steplist) = 
    let val atoms   = findAtoms (form::forms) []
        val curls   = wrap atoms ("{", "}")
        val squares = wrap atoms ("[", "]")
        val proof   = " proof"^(bracket (sequentToBoxProof seq))^" = "
    in
        "%abbrev\n"^title^":\n"^curls^proof^squares^"\n"^
        (proofstepsToBoxProof (steplist, []))(**)
    end;
