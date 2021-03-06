structure Auxiliaries = struct
open Proof;

(* fn: 'a * 'a list -> bool *)
fun member (element, set) = List.exists (fn e => (element = e)) set

(* fn bool*outstream*string -> bool *)
(* Returns given booliean, with the side-effect of outputting the given 
 * string to the given outstream when the boolean is false *)
fun feedback (true, _, _)           = true
  | feedback (false, out, message)  = (TextIO.output(out, message); false)

(* bool * string -> string *)
(* Returns the proper line prefix based on current validity *)
fun getPrefix(true, "")     = "\n[Conclusion]: "
  | getPrefix(true, self)   = "\n["^self^"]: "
  | getPrefix(false, _)     = " ";

(* fn: string -> unit *)
fun printl s = print(s^"\n");

(* fn: string -> string *)
fun bracket(s) = "("^s^")";

(* fn: formula -> string *)
fun formulaToString(Atom s)         = s
  | formulaToString(BOT)            = "\226\138\165"
  | formulaToString(NEG f)          = "\194\172"^negToString(f)
  | formulaToString(AND (f1,f2))    = andToString(f1)^"\226\136\167"^
                                      negToString(f2)
  | formulaToString(OR (f1, f2))    = orToString(f1)^"\226\136\168"^
                                      andToString(f2)
  | formulaToString(IMP (f1, f2))   = orToString(f1)^"\226\134\146"^
                                      formulaToString(f2)

and negToString(a as Atom _)        = formulaToString(a)
  | negToString(b as BOT)           = formulaToString(b)
  | negToString(n as NEG _)         = formulaToString(n)
  | negToString(f as _)             = bracket(formulaToString(f))

and andToString(f as OR _)          = bracket(formulaToString(f))
  | andToString(f as IMP _)         = bracket(formulaToString(f))
  | andToString(f as _)             = formulaToString(f)

and orToString(f as IMP _)         = bracket(formulaToString(f))
  | orToString(f as _)             = formulaToString(f);

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

fun ruleToString Cpy = "the copy rule"
  | ruleToString Ain = "the and-introduction rule"
  | ruleToString Ae1 = "the first and-elimination rule"
  | ruleToString Ae2 = "the second and-elimination rule"
  | ruleToString Oi1 = "the first or-introduction rule"
  | ruleToString Oi2 = "the second or-introduction rule"
  | ruleToString Oel = "the or-elimination rule"
  | ruleToString Iin = "the implication-introduction rule"
  | ruleToString Iel = "the implication-elimination rule"
  | ruleToString Nin = "the negation-introduction rule"
  | ruleToString Nel = "the negation-elimination rule"
  | ruleToString Din = "introduction of double-negation"
  | ruleToString Del = "elimination of double-negation"
  | ruleToString Bel = "elimination of absurdity"
  | ruleToString Mod = "Modus Tollens"
  | ruleToString Pbc = "proof by contradiction"
  | ruleToString Lem = "the law of the excluded middle"
  (* dummies for structural rules to exhaust patternmatching *)
  | ruleToString Prm = "premise"   
  | ruleToString Ass = "assumption"
  | ruleToString Dis = "discharge"
  ;


(* fn: proofstep * char list -> string *)
(* Patternmatching only gurantees acceptance of valid proofs, since it is 
 * only to be used with validated proofs *)
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
  | proofstepsToString ((SOME con, Cpy, [Line org], self)::steps, tabs) =
        "\n"^(implode tabs)^"By copying ["^org^"], we get "^
        (formulaToString con)^" ["^self^"]."
        ^(proofstepsToString (steps, tabs))
  | proofstepsToString ((SOME con, rule, refs, self)::steps, tabs)      =
        let val (verb,post) = case steps of [] => ("conclude ", ".")
                                          |  _ => ("get ", " ["^self^"].")
            val prepphrase  = if (rule = Lem) then "" 
                              else " to "^(refsToString refs)
        in
            "\n"^(implode tabs)^"By applying "^(ruleToString rule)^
            prepphrase^", we "^verb^(formulaToString con)^post
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
  | formulaToBoxProof(OR (f1, f2))    = orToBoxProof(f1)^" \\/ "^
                                      andToBoxProof(f2)
  | formulaToBoxProof(IMP (f1, f2))   = orToBoxProof(f1)^" => "^
                                      formulaToBoxProof(f2)

and negToBoxProof(a as Atom _)        = formulaToBoxProof(a)
  | negToBoxProof(b as BOT)           = formulaToBoxProof(b)
  | negToBoxProof(n as NEG _)         = formulaToBoxProof(n)
  | negToBoxProof(f as _)             = bracket(formulaToBoxProof(f))

and andToBoxProof(f as OR _)          = bracket(formulaToBoxProof(f))
  | andToBoxProof(f as IMP _)         = bracket(formulaToBoxProof(f))
  | andToBoxProof(f as _)             = formulaToBoxProof(f)

and orToBoxProof(f as IMP _)         = bracket(formulaToBoxProof(f))
  | orToBoxProof(f as _)             = formulaToBoxProof(f)
  ;

fun referenceToBoxProof (Line s)      = "@"^s
  | referenceToBoxProof (Box (a,c))   = "#"^a^"-"^c
  ;

(* fn: reference list -> string *)
fun refsToBoxProof []             = ""
  | refsToBoxProof [r1]           = " "^(referenceToBoxProof r1)^" " 
  | refsToBoxProof (r1::refs)     = 
        " "^(referenceToBoxProof r1)^(refsToBoxProof refs)
  ;

(* fn: formula list -> string *)
fun premisesToBoxProof []             = ""
  | premisesToBoxProof (f1::refs)     = 
        (formulaToBoxProof f1)^" , "^(premisesToBoxProof refs)

fun sequentToBoxProof (forms, f)  =
        (premisesToBoxProof forms)^" |- "^(formulaToBoxProof f);

fun ruleToBoxProof r =
    case r of Prm => "premise"
            | Ass => "assumption"
            | Dis => raise Match (* specify exception? *)
            | Cpy => "copy"
            | Ain => "con_i"
            | Ae1 => "con_e1"
            | Ae2 => "con_e2"
            | Oi1 => "dis_i1"
            | Oi2 => "dis_i2"
            | Oel => "dis_e"
            | Iin => "imp_i"
            | Iel => "imp_e"
            | Nin => "neg_i"
            | Nel => "neg_e"
            | Din => "nni"
            | Del => "nne"
            | Bel => "bot_e"
            | Mod => "mt"
            | Pbc => "pbc"
            | Lem => "lem"
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
            val argument  =
                let val preposition = if (member (rule, [Prm, Ass]))
                                      then " "
                                      else " by "
                in
                    preposition^(ruleToBoxProof rule)^(refsToBoxProof refs)
                end
        in
            "\n"^start^(formulaToBoxProof con)^argument^post
            ^(proofstepsToBoxProof (rest, indent))
        end
  | proofstepsToBoxProof ((NONE,_, _,_)::steps, _) = raise Match
  ;

fun wrap cs (s1, s2) = 
    foldr (fn (a,b) => a^b) "" (map (fn c => s1^c^s2) cs);

fun findAtoms [] l      = l 
  | findAtoms (f::fs) l = 
    let val here        = case f of
          Atom a        => if member(a, l) then l else a::l
        | BOT           => l
        | NEG  n        => findAtoms [n] l
        | AND (f1, f2)  => findAtoms [f1] (findAtoms [f2] l)
        | IMP (f1, f2)  => findAtoms [f1] (findAtoms [f2] l)
        | OR  (f1, f2)  => findAtoms [f1] (findAtoms [f2] l)
    in
        findAtoms fs here
    end
  ;

(* fn: proof -> string *)
fun toBoxProof (title, seq as (forms, form), steplist) = 
    let val atoms   = findAtoms (form::forms) []
        val curls   = wrap atoms ("{", "}")
        val squares = wrap atoms ("[", "]")
        val proof   = " proof"^(bracket (sequentToBoxProof seq))^" = "
    in
        "%abbrev\n"^title^":\n"^curls^proof^squares^"\n"^
        (proofstepsToBoxProof (steplist, []))^"\n"(**)
    end;

(* fn: unit -> proof *)
fun dummyinput () =
    ("boxproof",
     ([],OR(IMP(Atom("p"),Atom("q")),IMP(Atom("q"),Atom("r")))),
     [(SOME(OR(Atom("q"),NEG(Atom("q")))),Lem,[],"1"),
      (SOME(Atom("q")),Ass,[],"2"),
      (SOME(Atom("p")),Ass,[],"3"),
      (SOME(Atom("q")),Cpy,[Line("2")],"4"),
      (NONE,Dis,[Line("3")],""),
      (SOME(IMP(Atom("p"),Atom("q"))),Iin,[Box("3","4")],"5"),
      (SOME(OR(IMP(Atom("p"),Atom("q")),IMP(Atom("q"),Atom("r")))),
            Oi1,[Line("5")],"6"),
      (NONE,Dis,[Line("2")],""),
      (SOME(NEG(Atom("q"))),Ass,[],"7"),
      (SOME(Atom("q")),Ass,[],"8"),
      (SOME(BOT),Nel,[Line("8"),Line("7")],"9"),
      (SOME(Atom("r")),Bel,[Line("9")],"10"),
      (NONE,Dis,[Line("8")],""),
      (SOME(IMP(Atom("q"),Atom("r"))),Iin,[Box("8","10")],"11"),
      (SOME(OR(IMP(Atom("p"),Atom("q")),IMP(Atom("q"),Atom("r")))),
            Oi2,[Line("11")],"12"),
      (NONE,Dis,[Line("7")],""),
      (SOME(OR(IMP(Atom("p"),Atom("q")),IMP(Atom("q"),Atom("r")))),
            Oel,[Line("1"),Box("2","6"),Box("7","12")],"")
      ]
    )
;
end
