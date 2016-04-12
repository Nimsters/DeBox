use "datatypes.ml";

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
  | toProp(f as IMP _)    = impToProp(f)

and negToProp(f) =
    let val prop = toProp(f)
    in
        case prop of PRPU u => PRPU (NOT u)
                   | PRPC c => PRPN (NEGP (PCC c))
                   | PRPI i => PRPN (NEGP (PCI i))
                   | PRPN n => PRPN (NEGP (PCN n))
    end

(*
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

and impToProp(IMP(f1,f2))     = 
    let val left  = case (toProp f1) of PRPU u      => SU u
                                      | PRPN n      => SN n
                                      | PRPP p      => PP p
                                      | PRPC (CL l) => PL l
                                      | PRPC (CM m) => PM m
                                      | PRPC c      => STAT c
        
        val right = case (toProp f2) of PRPU u      => SU u
                                      | PRPN n      => SN n
                                      | PRPP p      => PP p
                                      | PRPC (CL l) => PL l
                                      | PRPC (CM m) => PM m
                                      | PRPC (CI i) => i
        val imp =
        case (left,right) of (Single s1, Single s2) => SIMPS (s1, s2)
                        | (Single s, Implication i) => SIMPI (s, i)
                        | (Single s, Prop p)        => SIMP  (s, p)
                        | (Prop p, Implication i)   => PIMPI (p, i)
                        | (Prop p, Single s)        => PIMPS (p,  PSS s)
                        | (Prop p1, Prop p2)        => PIMPS (p1, PSP p2)
    in
        PRPC (CI imp)
    end
 
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
*)
