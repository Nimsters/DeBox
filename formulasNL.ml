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

and andToProp(AND(AND(f1,f2),f3))   =
        let val e1 = toAext f1
            val e2 = toElement f2
            val e3 = toElement f3
        in
            PRPC (CC (CL (AL (EEX (e1, e2), e3))))
        end

  | andToProp(AND(f1,f2))           =
        let val e1 = toElement f1
            val e2 = toElement f2
        in
            case (e1, e2) of
                (ELS s1, ELS s2)    => PRPC (CP (ASS (s1,s2)))
                (ELS s1, ELP p2)    => PRPC (CC (CM (ASE (s1, EP p2))))
                (ELS s1, ELI i2)    => PRPC (CC (CM (ASE (s1, EI i2))))
                (ELP p1, _)         => PRPC (CC (CM (APE (p1, e2)
                (ELI i1, _)         => PRPC (CC (CM (AIE (STTI i1, e2))))
        end

and toAext(AND (f1,f2)) = EXT (EEX (toAext f1, toElement f2))
  | toAext f            = EL (toElement f)           

and orToProp(OR(OR(f1,f2),f3))   =
        let val e1 = toOext f1
            val e2 = toElement f2
            val e3 = toElement f3
        in
            PRPC (CC (CL (OL (EEX (e1, e2), e3))))
        end

  | orToProp(OR(f1,f2))             =
        let val e1 = toElement f1
            val e2 = toElement f2
        in
            case (e1, e2) of
                (ELS s1, ELS s2)    => PRPC (CP (OSS (s1,s2)))
                (ELS s1, ELP p2)    => PRPC (CC (CM (OSE (s1, EP p2))))
                (ELS s1, ELI i2)    => PRPC (CC (CM (OSE (s1, EI i2))))
                (ELP p1, _)         => PRPC (CC (CM (OPE (p1, e2)
                (ELI i1, _)         => PRPC (CC (CM (OIE (STTI i1, e2))))
        end

and toOext(OR (f1,f2))  = EXT (EEX (toOext f1, toElement f2))
  | toOext f            = EL (toElement f)           

and toElement(f) = case (toProp f) of PRPU u        => ELS (SU u)
                                    | PRPN n        => ELS (SN n)
                                    | PRPI i        => ELI i
                                    | PRPC (CP p)   => ELP p
                                    | PRPC (CC c)   => ELS (STTC c)

(*
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
*) 

