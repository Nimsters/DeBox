datatype Proposition = PRPC of Complex  | PRPI  of Implication | 
                       PRPN of Negation | PRPU  of Unit
and      PropC       = PCC  of Complex  | PCI   of Implication | 
                       PCN  of Negation
and      PropS       = PSC  of Complex  | PSS   of Single
and      Prop        = PC   of Complex  | PI    of Imp
and      Complex     = CC   of Comp     | CP    of Pair 
and      Comp        = CL   of List     | CM    of Multi
and      Imp         = STTI of Implication
and      Implication = ISS  of Single*Single    | ISI of Single*Implication |
                       ISC  of Single*Complex   | IPP of Prop*PropS | 
                       IPI  of Prop*Implication
and      List        = AL   of Extend*Element   | OL  of Extend*Element
and      Multi       = APE  of Pair*Element     | ASE of Single*Elem |
                       OPE  of Pair*Element     | OSE of Single*Elem |
                       AIE  of Imp*Element      | OIE of Imp*Element
and      Extend      = EEX  of Ext*Element 
and      Ext         = EL   of Element | EXT   of Extend 
and      Element     = ELP  of Pair    | ELI   of Implication | ELS of Single
and      Elem        = EP   of Pair    | EI    of Implication
and      Pair        = ASS  of Single*Single    | OSS  of Single*Single 
and      Single      = STTC of Comp    | SN of Negation       | SU of Unit
and      Negation    = NEGP of PropC
and      Unit        = NOT  of Unit | atom of char;
