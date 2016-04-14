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
              | (ELS s1, ELP p2)    => PRPC (CC (CM (ASE (s1, EP p2))))
              | (ELS s1, ELI i2)    => PRPC (CC (CM (ASE (s1, EI i2))))
              | (ELP p1, _)         => PRPC (CC (CM (APE (p1, e2))))
              | (ELI i1, _)         => PRPC (CC (CM (AIE (STTI i1, e2))))
        end

and toAext(AND (f1,f2)) = EXT (EEX (toAext f1, toElement f2))
  | toAext f            = EL (toElement f)           

and orToProp(OR(OR(f1,f2),f3))      =
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
              | (ELS s1, ELP p2)    => PRPC (CC (CM (OSE (s1, EP p2))))
              | (ELS s1, ELI i2)    => PRPC (CC (CM (OSE (s1, EI i2))))
              | (ELP p1, _)         => PRPC (CC (CM (OPE (p1, e2))))
              | (ELI i1, _)         => PRPC (CC (CM (OIE (STTI i1, e2))))
        end

and toOext(OR (f1,f2))  = EXT (EEX (toOext f1, toElement f2))
  | toOext f            = EL (toElement f)           

and toElement(f) = case (toProp f) of PRPU u        => ELS (SU u)
                                    | PRPN n        => ELS (SN n)
                                    | PRPI i        => ELI i
                                    | PRPC (CP p)   => ELP p
                                    | PRPC (CC c)   => ELS (STTC c)

and impToProp(IMP(f1,f2))     = 
    let val prop1 = toProp f1
        val prop2 = toProp f2
    in
        case (prop1, prop2) of 
             (PRPI i1, PRPI i2)     => PRPI (IPI (PI (STTI i1), i2))
           | (PRPC c1, PRPI i2)     => PRPI (IPI (PC c1, i2))
           | (PRPI i1, PRPC c2)     => PRPI (IPP (PI (STTI i1), PSC c2))
           | (PRPC c1, PRPC c2)     => PRPI (IPP (PC c1, PSC c2))
           | (PRPI i1, PRPU u2)     => PRPI (IPP (PI (STTI i1), PSS (SU u2)))
           | (PRPC c1, PRPU u2)     => PRPI (IPP (PC c1, PSS (SU u2)))
           | (PRPI i1, PRPN n2)     => PRPI (IPP (PI (STTI i1), PSS (SN n2)))
           | (PRPC c1, PRPN n2)     => PRPI (IPP (PC c1, PSS (SN n2)))
           | (PRPU u1, PRPI i2)     => PRPI (ISI (SU u1, i2))
           | (PRPN n1, PRPI i2)     => PRPI (ISI (SN n1, i2))
           | (PRPU u1, PRPC c2)     => PRPI (ISC (SU u1, c2))
           | (PRPN n1, PRPC c2)     => PRPI (ISC (SN n1, c2))
           | (PRPU u1, PRPU u2)     => PRPI (ISS (SU u1, SU u2))
           | (PRPN n1, PRPU u2)     => PRPI (ISS (SN n1, SU u2))
           | (PRPU u1, PRPN n2)     => PRPI (ISS (SU u1, SN n2))
           | (PRPN n1, PRPN n2)     => PRPI (ISS (SN n1, SN n2))
    end
      

