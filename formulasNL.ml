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
      
fun propositionNL (PRPC c)      = complexNL c
  | propositionNL (PRPI i)      = implicationNL i
  | propositionNL (PRPN n)      = negationNL n
  | propositionNL (PRPU u)      = unitNL u

and propCNL (PCC  c)            = complexNL c
  | propCNL (PCI  i)            = implicationNL i
  | propCNL (PCN  n)            = negationNL n

and propSNL (PSC  c)            = complexNL c
  | propSNL (PSS  s)            = singleNL s

and propNL (PC   c)             = complexNL c
  | propNL (PI   i)             = impNL i

and complexNL (CC   c)          = compNL c
  | complexNL (CP   p)          = pairNL p

and compNL (CL   l)             = listNL l
  | compNL (CM   m)             = multiNL m

and impNL (STTI  i)             = "the implication \""^(implicationNL i)^"\""

and implicationNL (ISS (l, r))  = (singleNL l)^" implies "^(singleNL r)
  | implicationNL (ISI (s, i))  = 
            (singleNL s)^" implies that "^(implicationNL i)
  | implicationNL (ISC (s, c))  = 
            "if "^(singleNL s)^" is provable then so is "^(complexNL c)
  | implicationNL (IPP (l, r))  = 
            "if "^(propNL l)^" is provable then so is "^(propSNL r)
  | implicationNL (IPI (p, i))  =
            "if "^(propNL p)^" is provable then so is the proposition that "
            ^(implicationNL i)

and listNL (AL  (x, e)) = (extendNL x)^   ", and also "   ^(elementNL e)
  | listNL (OL  (x, e)) = (extendNL x)^   ", or else "    ^(elementNL e)

and multiNL (APE (p, e)) = (pairNL p)^   " and also "    ^(elementNL e)
  | multiNL (ASE (s, e)) = (singleNL s)^   " and also "    ^(elemNL e)
  | multiNL (OPE (p, e)) = (pairNL p)^   " or else "     ^(elementNL e)
  | multiNL (OSE (s, e)) = (singleNL s)^   " or else "     ^(elemNL e)
  | multiNL (AIE (i, e)) = (impNL i)^   " and also "    ^(elementNL e)
  | multiNL (OIE (i, e)) = (impNL i)^   " or else "     ^(elementNL e)

and extendNL (EEX (x, e)) = (extNL x)^   ", "            ^(elementNL e)

and extNL (EL   e)     = elementNL e
  | extNL (EXT  e)     = extendNL e

and elementNL (ELP  p)     = pairNL p
  | elementNL (ELI  i)     = implicationNL i
  | elementNL (ELS  s)     = singleNL s

and elemNL (EP   p)     = pairNL p
  | elemNL (EI   i)     = implicationNL i

and pairNL (ASS (l, r)) = (singleNL l)^   " and "         ^(singleNL r)
  | pairNL (OSS (l, r)) = (singleNL l)^   " or "          ^(singleNL r)

and singleNL (STTC c)     = "the statement \""    ^(compNL c)^    "\""
  | singleNL (SN   n)     = negationNL n
  | singleNL (SU   u)     = unitNL u

and negationNL (NEGP p)     = "the negation of \""  ^(propCNL p)^    "\""

and unitNL (NOT  u)     = "not-"                ^(unitNL u)
  | unitNL (atom c)     = Char.toString c


fun toFormula (PRPC c)  = complexF c
  | toFormula (PRPI i)  = implicationF i
  | toFormula (PRPN n)  = negationF n
  | toFormula (PRPU u)  = unitF u

and propCF (PCC  c)     = complexF c
  | propCF (PCI  i)     = implicationF i
  | propCF (PCN  n)     = negationF n

and propSF (PSC  c)     = complexF c
  | propSF (PSS  s)     = singleF s

and propF (PC   c)      = complexF c
  | propF (PI   i)      = impF i

and complexF (CC   c)   = compF c
  | complexF (CP   p)   = pairF p

and compF (CL   l)      = listF l
  | compF (CM   m)      = multiF m

and impF (STTI  i)      = implicationF i

and implicationF (ISS (l, r))  = IMP (singleF l, singleF r)
  | implicationF (ISI (s, i))  = IMP (singleF s, implicationF i)
  | implicationF (ISC (s, c))  = IMP (singleF s, complexF c)
  | implicationF (IPP (l, r))  = IMP (propF l, propSF r)
  | implicationF (IPI (p, i))  = IMP (propF p, implicationF i)

and listF (AL  (x, e))  = AND (extendA x, elementF e)
  | listF (OL  (x, e))  = OR (extendO x, elementF e)

and multiF (APE (p, e)) = AND (pairF p, elementF e)
  | multiF (ASE (s, e)) = AND (singleF s, elemF e)
  | multiF (OPE (p, e)) = OR (pairF p, elementF e)
  | multiF (OSE (s, e)) = OR (singleF s, elemF e)
  | multiF (AIE (i, e)) = AND (impF i, elementF e)
  | multiF (OIE (i, e)) = OR (impF i, elementF e)

and extendA (EEX (x,e)) = AND (extA x, elementF e)
and extendO (EEX (x,e)) = OR (extO x, elementF e)

and extA (EL   e)       = elementF e
  | extA (EXT  e)       = extendA e

and extO (EL   e)       = elementF e
  | extO (EXT  e)       = extendO e

and elementF (ELP  p)   = pairF p
  | elementF (ELI  i)   = implicationF i
  | elementF (ELS  s)   = singleF s

and elemF (EP p)        = pairF p
  | elemF (EI i)        = implicationF i

and pairF (ASS (l, r))  = AND (singleF l, singleF r)
  | pairF (OSS (l, r))  = OR (singleF l, singleF r)

and singleF (STTC c)    = compF c
  | singleF (SN   n)    = negationF n
  | singleF (SU   u)    = unitF u

and negationF (NEGP p)  = NEG (propCF p)

and unitF (NOT  u)      = NEG (unitF u)
  | unitF (atom c)      = Atom c
