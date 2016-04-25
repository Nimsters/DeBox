use "datatypes.ml";

fun bracket(s) = "("^s^")";

fun formulaToString(Atom c)        = Char.toString c
  | formulaToString(NEG f)         = "¬"^negToString(f)
  | formulaToString(AND (f1,f2))  = andToString(f1)^"/\\"^negToString(f2)
  | formulaToString(OR (f1, f2))  = impToString(f1)^"\\/"^andToString(f2)
  | formulaToString(IMP (f1, f2))  = impToString(f1)^"→"^formulaToString(f2)

and negToString(a as Atom _)    = formulaToString(a)
  | negToString(n as NEG _)     = formulaToString(n)
  | negToString(f as _)         = bracket(formulaToString(f))

and andToString(f as OR _)     = bracket(formulaToString(f))
  | andToString(f as IMP _)     = bracket(formulaToString(f))
  | andToString(f as _)         = formulaToString(f)

and impToString(f as IMP _)      = bracket(formulaToString(f))
  | impToString(f as _)          = formulaToString(f);

fun printl s = print(s^"\n");

fun sqBracket s = "["^s^"]";

fun referenceToString (Line s)      = sqBracket s
  | referenceToString (Box (s1,s2)) = (sqBracket s1)^"-"^(sqBracket s2);

fun selfToString (Line s)       = " "^(sqBracket s)
  | selfToString (Box (s1, s2)) = " "^(sqBracket s2)^
                                  ", from the assumption "^(sqBracket s1)
  | selfToString Conclusion     = "";

fun premisesToString ([f])    = "and "^formulaToString f
  | premisesToString (f::fs)  = (formulaToString f)^", "^premisesToString fs

fun sequentToString (Sequent ([], f))       =
        "Without premises, we wish to prove "^(formulaToString f)^"."
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

fun proofstepToString (Step(con, Ass, refs, self))    =
    "We now assume "^(formulaToString con)^(selfToString self)
  | proofstepToString (Step(con, Prm, refs, self))    =
    "We have "^(formulaToString con)^" as a premise"^
    (selfToString self)
  | proofstepToString (Step(con, Cpy, refs, self))    =
    let val original = case refs of []      => ""
                                  | r::rs   => referenceToString r
    in
        "By copying "^original^", we get "^(formulaToString con)^" here too "
        ^(selfToString self)
    end
  | proofstepToString (Step(con, r, refs, self)) =
    let val rStr = case refs of []        => ""
                              | [r0]      => " to "^(referenceToString r0)
                              | [r1, r2]  => " to "^(referenceToString r1)^
                                            " and "^(referenceToString r2)
        val wording = 
            case self of Line _     => ", we get "
                       | Box  _     => ", we get the part-conclusion "
                       | Conclusion => ", we may conclude "
    in
        "By applying "^(ruleToString r)^rStr^wording^
        (formulaToString con)^(selfToString self)
    end

fun proofToString (Proof (title, seq, pslist))  =
   "\n"^title^":\n"^(sequentToString seq)^" "^
   (foldl (fn (s, b) => b^s^". ") "" (map proofstepToString pslist))^"\n"; 
