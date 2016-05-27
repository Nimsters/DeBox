local
in
datatype token =
    AE1 of unit
  | AE2 of unit
  | AIN of unit
  | AND of unit
  | ASS of unit
  | ATOM of string
  | BEL of unit
  | BOT of unit
  | COLON of unit
  | COMMA of unit
  | CPY of unit
  | DASH of unit
  | DEL of unit
  | DIN of unit
  | DIS of unit
  | DOT of unit
  | EOF of unit
  | FROM of unit
  | GET of unit
  | ID of string
  | IEL of unit
  | IIN of unit
  | IMP of unit
  | LBRA of unit
  | LEM of unit
  | LPAR of unit
  | MOD of unit
  | NEG of unit
  | NEL of unit
  | NIN of unit
  | OEL of unit
  | OI1 of unit
  | OI2 of unit
  | OR of unit
  | PBC of unit
  | PLUS of unit
  | PREM of unit
  | PRM of unit
  | PRMS of unit
  | RBRA of unit
  | RPAR of unit
  | TO of unit
  | WC of unit
  | WG of unit
  | WTP of unit
  | WWTP of unit
end;

val Proof :
  (Lexing.lexbuf -> token) -> Lexing.lexbuf -> Proof.proof;
