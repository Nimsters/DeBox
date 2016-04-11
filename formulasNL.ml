datatype formula = Atom of char |
                   NEG of formula |
                   AND of formula*formula |
                   OR of formula*formula |
                   IMP of formula*formula;
                   
datatype Proposition = PRPC of Complex | PRPP of Pair | PRPN of Negation 
                     | PRPU of Unit
and      Prop        = PL of List  | PM of Multi  | PP of Pair
and      PropS       = PSL of List | PSM of Multi | PSP of  Pair 
                     | PSS of Single
and      PropC       = PCC of Complex | PCP of Pair | PCN of Negation
and      Complex     = CI of Implication | CL of List | CM of Multi
and      Implication = SIMPS of Single*Single | SIMPI of Single*Implication |
                       PIMPS of Prop*PropS | SIMP of PropS*Prop |
                       PIMPI of Prop*Implication
and      List        = ALE of Element*Element*Element |
                       OLE of Element*Element*Element | L of Element*List
and      Multi       = ANDPE of Pair*Element | ANDSE of Single*Elem |
                       ORPE  of Pair*Element | ORSE  of Single*Elem
and      Element     = ELMP of Pair | ELMI of Implication | ELMS of Single
and      Elem        = EP of Pair | EI of Implication
and      Pair        = ANDS of Single*Single | ORS of Single*Single
and      Single      = STAT of Complex | SN of Negation | SU of Unit
and      Negation    = NEGP of PropC
(*and      Neg         = NEGU of Unit*)
and      Unit        = NOT of Unit | atom of char;

fun bracket(s) = "("^s^")";

fun toString(Atom c)        = Char.toString c
  | toString(NEG f)         = "¬"^negToString(f)
  | toString(AND (f1,f2))  = andToString(f1)^"/\\"^negToString(f2)
  | toString(OR (f1, f2))  = impToString(f1)^"\\/"^andToString(f2)
  | toString(IMP (f1, f2))  = impToString(f1)^"=>"^toString(f2)

and negToString(a as Atom _)    = toString(a)
  | negToString(n as NEG _)     = toString(n)
  | negToString(f as _)         = bracket(toString(f))

and andToString(f as OR _)     = bracket(toString(f))
  | andToString(f as IMP _)     = bracket(toString(f))
  | andToString(f as _)         = toString(f)

and impToString(f as IMP _)      = bracket(toString(f))
  | impToString(f as _)          = toString(f);

fun printl s = print(s^"\n");

fun toProp(Atom c)        = PRPU (atom c)
  | toProp(NEG f)         = negToProp(f)
  | toProp(f as AND _)    = andToProp(f)
  | toProp(f as OR _)     = orToProp(f)
 (* | toProp(IMP (f1, f2))  = impToNL(f1)^" implies that "^toNL(f2) *)

and negToProp(f) =
    let val prop = toProp(f)
    in
        case prop of PRPU u => PRPU (NOT u)
                   | PRPC c => PRPN (NEGP (PCC c))
                   | PRPP p => PRPN (NEGP (PCP p))
                   | PRPN n => PRPN (NEGP (PCN n))
    end

and andToProp(f as AND(AND (AND _, _), _)) = toAlist
  | andToProp(AND(f1,f2))     = 
          let val left  = case (toProp f1) of PRPU u => SU u
                                            | PRPN n => SN n
                                            | PRPC c => STAT c
                                            | PRPP p => p
              
              val right = case (toProp f2) of PRPU u => SU u
                                            | PRPN n => SN n
                                            | PRPP p => p
                                            | PRPC (CI i) => i
                                            | PRPC c => STAT c
        in
          case (left, right) of (Single s1, Single s2) => PRPP (ANDS (s1,s2))
               | (Single s, Pair p) => PRPC (CM (ANDSE (s, EP p)))
               | (Single s, Implication i) => PRPC (CM (ANDSE (s, EI i)))
               | (Pair p, Single s) => PRPC (CM (ANDPE (p,  ELMS s)))
               | (Pair p1, Pair p2) => PRPC (CM (ANDPE (p1, ELMP p2)))
               | (Pair p, Implication i) => PRPC (CM (ANDPE (p, ELMI i)))
        end

and orToProp(f as OR(OR (OR _), _)) = toOlist(f)
  | orToProp(f as OR(f1,f2))     = 
          let val left  = case (toProp f1) of PRPU u => SU u
                                            | PRPN n => SN n
                                            | PRPC c => STAT c
                                            | PRPP p => p
              
              val right = case (toProp f2) of PRPU u => SU u
                                            | PRPN n => SN n
                                            | PRPP p => p
                                            | PRPC (CI i) => i
                                            | PRPC c => STAT c
        in
          case (left, right) of (Single s1, Single s2) => PRPP (ORS (s1,s2))
               | (Single s, Pair p) => PRPC (CM (ORSE (s, EP p)))
               | (Single s, Implication i) => PRPC (CM (ORSE (s, EI i)))
               | (Pair p, Single s) => PRPC (CM (ORPE (p,  ELMS s)))
               | (Pair p1, Pair p2) => PRPC (CM (ORPE (p1, ELMP p2)))
               | (Pair p, Implication i) => PRPC (CM (ORPE (p, ELMI i)))
        end

(*
and impToNL(f as IMP _)      = bracket(toNL(f))
  | impToNL(f as _)          = toNL(f); *)

and toAlist(AND (AND (AND (f, f1), f2),f3)) =
    let val elm1 = toElement(f1)
        val elm2 = toElement(f2)
        val elm3 = toElement(f3)
    in
        extendAlist(f, ALE (elm1, elm2, elm3))
    end

and extendAlist(AND (f1, f2), alist) =
    extendAlist(f1, L(toElement(f2), alist))
  | extendAlist(f, alist) = PRPC (CL (L (toElement(f), alist)))

and toOlist(OR (OR (OR (f, f1), f2),f3)) =
    let val elm1 = toElement(f1)
        val elm2 = toElement(f2)
        val elm3 = toElement(f3)
    in
        extendOlist(f, OLE (elm1, elm2, elm3))
    end

and extendOlist(OR (f1, f2), olist) =
    extendOlist(f1, L(toElement(f2), olist))
  | extendOlist(f, alist) = PRPC (CL (L (toElement(f), olist)))

and toElement(f) = case (toProp f) of PRPU u => ELMS (SU u)
                                    | PRPN n => ELMS (SN n)
                                    | PRPP p => ELMP p
                                    | PRPC (CI i) => ELMI i
                                    | PRPC c => ELMS (STAT c)


(* Tests *)
local
val p = Atom #"p"
val q = Atom #"q"
val l = Atom #"l"
val r = Atom #"r"
val s = Atom #"s"
val test = []
(* Add new cases here at the top! *)
(* Examples from HR p.81 *)
val test = AND(IMP(p,q),IMP(NEG r, OR(q, AND(NEG p, r))))::test (* l *)
val test = IMP(OR(IMP(s,OR(r,l)),AND(NEG q,r)),IMP(NEG(IMP(p,s)),r))::test
val test = OR(s,IMP(NEG p, NEG p))::test (* j *)
val test = IMP(OR(s, NEG p),NEG p)::test (* i *)
val test = IMP(AND(p,q),OR(NEG r, IMP(q,r)))::test (* h *)
val test = OR(NEG p, IMP(p,q))::test (* g *)
val test = NEG(AND(AND(NEG q, IMP(p,r)),IMP(r,q)))::test (* f *)
val test = IMP(p, OR(NEG q, IMP(q,p)))::test (* e *)
val test = AND(p, IMP(NEG q, NEG p))::test (* d *)
val test = IMP(AND(p,NEG q), NEG p)::test (* c *)
(* ------- *)
val test = NEG(IMP(p, IMP(q,r)))::test
val test = NEG(IMP(IMP(p,q),r))::test
val test = NEG(AND(p, OR(q,r)))::test
val test = NEG(OR(AND(p,q),r))::test
val test = NEG(AND(OR(p,q),r))::test
val test = NEG(OR(p, AND(q,r)))::test
val test = NEG(AND(AND(p,q),r))::test
val test = NEG(AND(p, AND(q,r)))::test
val test = IMP(p, IMP(q,r))::test
val test = IMP(IMP(p,q),r)::test
val test = AND(p, OR(q,r))::test
val test = OR(AND(p,q),r)::test
val test = AND(OR(p,q),r)::test
val test = OR(p, AND(q,r))::test
val test = OR(OR(p,q),r)::test
val test = OR(p, OR(q,r))::test
val test = AND(AND(p,q),r)::test
val test = AND(p, AND(q,r))::test
val test = NEG(NEG p)::test
val test = IMP(p,q)::test
val test = OR(p,q)::test
val test = AND(p,q)::test
val test = NEG(q)::test
val test = p::test
val t0::t1::rest = test
in
val test_toString = map printl (map toString test)
(* val test_toNL = map printl (map toNL test) *)
val test0_toProp = toProp t0
val test1_toProp = toProp t1
end;
quit();
