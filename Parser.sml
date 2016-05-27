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

open Obj Parsing;
prim_val vector_ : int -> 'a -> 'a Vector.vector = 2 "make_vect";
prim_val update_ : 'a Vector.vector -> int -> 'a -> unit = 3 "set_vect_item";

val yytransl = #[
  257 (* AE1 *),
  258 (* AE2 *),
  259 (* AIN *),
  260 (* AND *),
  261 (* ASS *),
  262 (* ATOM *),
  263 (* BEL *),
  264 (* BOT *),
  265 (* COLON *),
  266 (* COMMA *),
  267 (* CPY *),
  268 (* DASH *),
  269 (* DEL *),
  270 (* DIN *),
  271 (* DIS *),
  272 (* DOT *),
  273 (* EOF *),
  274 (* FROM *),
  275 (* GET *),
  276 (* ID *),
  277 (* IEL *),
  278 (* IIN *),
  279 (* IMP *),
  280 (* LBRA *),
  281 (* LEM *),
  282 (* LPAR *),
  283 (* MOD *),
  284 (* NEG *),
  285 (* NEL *),
  286 (* NIN *),
  287 (* OEL *),
  288 (* OI1 *),
  289 (* OI2 *),
  290 (* OR *),
  291 (* PBC *),
  292 (* PLUS *),
  293 (* PREM *),
  294 (* PRM *),
  295 (* PRMS *),
  296 (* RBRA *),
  297 (* RPAR *),
  298 (* TO *),
  299 (* WC *),
  300 (* WG *),
  301 (* WTP *),
  302 (* WWTP *),
    0];

val yylhs = "\255\255\
\\001\000\002\000\002\000\003\000\003\000\004\000\004\000\005\000\
\\005\000\006\000\006\000\006\000\006\000\007\000\007\000\008\000\
\\008\000\009\000\009\000\013\000\010\000\014\000\015\000\016\000\
\\017\000\018\000\019\000\020\000\021\000\022\000\023\000\024\000\
\\025\000\026\000\027\000\028\000\029\000\011\000\012\000\012\000\
\\012\000\012\000\012\000\012\000\012\000\000\000";

val yylen = "\002\000\
\\004\000\003\000\004\000\002\000\002\000\003\000\003\000\001\000\
\\002\000\003\000\003\000\004\000\002\000\006\000\003\000\001\000\
\\001\000\003\000\003\000\000\000\011\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\033\000\001\000\001\000\
\\003\000\003\000\003\000\002\000\003\000\002\000";

val yydefred = "\000\000\
\\000\000\000\000\000\000\046\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\040\000\039\000\000\000\000\000\000\000\
\\022\000\008\000\001\000\000\000\000\000\000\000\005\000\000\000\
\\000\000\000\000\000\000\000\000\002\000\000\000\000\000\000\000\
\\009\000\000\000\000\000\000\000\000\000\013\000\000\000\017\000\
\\000\000\000\000\000\000\000\000\045\000\000\000\000\000\000\000\
\\023\000\000\000\000\000\000\000\000\000\010\000\011\000\000\000\
\\000\000\007\000\000\000\000\000\020\000\012\000\015\000\000\000\
\\019\000\000\000\018\000\024\000\000\000\000\000\000\000\000\000\
\\000\000\025\000\000\000\014\000\000\000\000\000\026\000\000\000\
\\000\000\000\000\027\000\000\000\000\000\021\000\028\000\000\000\
\\029\000\000\000\030\000\000\000\031\000\000\000\032\000\000\000\
\\033\000\000\000\034\000\000\000\035\000\000\000\036\000\000\000\
\\037\000\000\000\038\000";

val yydgoto = "\002\000\
\\004\000\008\000\011\000\023\000\019\000\020\000\038\000\039\000\
\\040\000\041\000\021\000\024\000\069\000\032\000\060\000\071\000\
\\077\000\081\000\085\000\088\000\090\000\092\000\094\000\096\000\
\\098\000\100\000\102\000\104\000\106\000";

val yysindex = "\017\000\
\\237\254\000\000\012\255\000\000\249\254\058\255\083\255\046\255\
\\083\255\083\255\236\254\000\000\000\000\083\255\083\255\013\255\
\\000\000\000\000\000\000\046\255\081\255\080\255\000\000\045\255\
\\083\255\004\255\000\000\083\255\000\000\083\255\083\255\031\255\
\\000\000\022\255\032\255\083\255\083\255\000\000\008\255\000\000\
\\248\254\083\255\083\255\080\255\000\000\000\000\080\255\092\255\
\\000\000\064\255\217\254\027\255\054\255\000\000\000\000\032\255\
\\032\255\000\000\080\255\061\255\000\000\000\000\000\000\086\255\
\\000\000\248\254\000\000\000\000\084\255\067\255\108\255\093\255\
\\099\255\000\000\078\255\000\000\087\255\104\255\000\000\096\255\
\\090\255\106\255\000\000\088\255\098\255\000\000\000\000\105\255\
\\000\000\109\255\000\000\101\255\000\000\103\255\000\000\119\255\
\\000\000\121\255\000\000\128\255\000\000\110\255\000\000\107\255\
\\000\000\111\255\000\000";

val yyrindex = "\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\094\255\000\000\000\000\
\\000\000\000\000\252\254\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\055\255\000\000\000\000\063\255\000\000\255\254\076\255\049\255\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\095\255\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000";

val yygindex = "\000\000\
\\000\000\000\000\000\000\096\000\121\000\000\000\231\255\108\000\
\\088\000\045\000\000\000\039\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000";

val YYTABLESIZE = 144;
val yytable = "\044\000\
\\003\000\056\000\043\000\036\000\037\000\044\000\044\000\028\000\
\\043\000\043\000\006\000\044\000\044\000\055\000\043\000\043\000\
\\028\000\001\000\044\000\044\000\005\000\043\000\043\000\054\000\
\\025\000\062\000\030\000\057\000\029\000\044\000\028\000\044\000\
\\043\000\049\000\043\000\030\000\044\000\031\000\007\000\043\000\
\\044\000\050\000\063\000\043\000\045\000\016\000\031\000\022\000\
\\028\000\030\000\036\000\037\000\026\000\027\000\042\000\034\000\
\\017\000\028\000\042\000\042\000\031\000\068\000\018\000\044\000\
\\042\000\042\000\046\000\030\000\047\000\048\000\016\000\042\000\
\\042\000\003\000\052\000\053\000\030\000\064\000\031\000\003\000\
\\043\000\059\000\042\000\028\000\042\000\041\000\041\000\031\000\
\\012\000\042\000\013\000\041\000\041\000\042\000\009\000\028\000\
\\010\000\016\000\016\000\041\000\066\000\067\000\030\000\061\000\
\\034\000\070\000\073\000\072\000\014\000\074\000\015\000\041\000\
\\075\000\031\000\076\000\080\000\041\000\078\000\079\000\082\000\
\\041\000\083\000\035\000\036\000\037\000\084\000\089\000\086\000\
\\087\000\091\000\093\000\095\000\097\000\099\000\101\000\107\000\
\\103\000\058\000\004\000\006\000\033\000\105\000\051\000\065\000";

val yycheck = "\004\001\
\\020\001\010\001\004\001\043\001\044\001\010\001\011\001\004\001\
\\010\001\011\001\018\001\016\001\017\001\039\000\016\001\017\001\
\\004\001\001\000\023\001\024\001\009\001\023\001\024\001\016\001\
\\045\001\051\000\023\001\036\001\016\001\034\001\004\001\036\001\
\\034\001\003\001\036\001\023\001\041\001\034\001\046\001\041\001\
\\045\001\020\001\016\001\045\001\041\001\007\000\034\001\009\000\
\\004\001\023\001\043\001\044\001\014\000\015\000\010\001\024\001\
\\011\001\004\001\010\001\011\001\034\001\001\001\017\001\025\000\
\\016\001\017\001\028\000\023\001\030\000\031\000\016\001\023\001\
\\024\001\011\001\036\000\037\000\023\001\024\001\034\001\017\001\
\\036\001\043\000\034\001\004\001\036\001\010\001\011\001\034\001\
\\006\001\041\001\008\001\016\001\017\001\045\001\037\001\004\001\
\\039\001\043\001\044\001\024\001\056\000\057\000\023\001\040\001\
\\024\001\020\001\040\001\024\001\026\001\002\001\028\001\036\001\
\\020\001\034\001\016\001\012\001\041\001\040\001\032\001\024\001\
\\045\001\032\001\042\001\043\001\044\001\020\001\022\001\040\001\
\\031\001\021\001\030\001\029\001\014\001\013\001\007\001\025\001\
\\027\001\042\000\045\001\045\001\020\000\035\001\035\000\056\000";

val yyact = vector_ 47 (fn () => ((raise Fail "parser") : obj));
(* Rule 1, file Parser.grm, line 28 *)
val _ = update_ yyact 1
(fn () => repr(let
val d__1__ = peekVal 3 : string
val d__2__ = peekVal 2 : unit
val d__3__ = peekVal 1 : Proof.sequent
val d__4__ = peekVal 0 : Proof.proofstep list
in
(((d__1__), (d__3__), (d__4__))) end : Proof.proof))
;
(* Rule 2, file Parser.grm, line 32 *)
val _ = update_ yyact 2
(fn () => repr(let
val d__1__ = peekVal 2 : unit
val d__2__ = peekVal 1 : Proof.formula
val d__3__ = peekVal 0 : unit
in
(([], (d__2__))) end : Proof.sequent))
;
(* Rule 3, file Parser.grm, line 33 *)
val _ = update_ yyact 3
(fn () => repr(let
val d__1__ = peekVal 3 : unit
val d__2__ = peekVal 2 : Proof.formula list
val d__3__ = peekVal 1 : unit
val d__4__ = peekVal 0 : Proof.formula
in
(((d__2__), (d__4__))) end : Proof.sequent))
;
(* Rule 4, file Parser.grm, line 37 *)
val _ = update_ yyact 4
(fn () => repr(let
val d__1__ = peekVal 1 : unit
val d__2__ = peekVal 0 : Proof.formula
in
([(d__2__)]) end : Proof.formula list))
;
(* Rule 5, file Parser.grm, line 38 *)
val _ = update_ yyact 5
(fn () => repr(let
val d__1__ = peekVal 1 : unit
val d__2__ = peekVal 0 : Proof.formula list
in
((d__2__)) end : Proof.formula list))
;
(* Rule 6, file Parser.grm, line 42 *)
val _ = update_ yyact 6
(fn () => repr(let
val d__1__ = peekVal 2 : Proof.formula
val d__2__ = peekVal 1 : unit
val d__3__ = peekVal 0 : Proof.formula
in
((d__1__) :: [(d__3__)]) end : Proof.formula list))
;
(* Rule 7, file Parser.grm, line 43 *)
val _ = update_ yyact 7
(fn () => repr(let
val d__1__ = peekVal 2 : Proof.formula
val d__2__ = peekVal 1 : unit
val d__3__ = peekVal 0 : Proof.formula list
in
((d__1__) :: (d__3__)) end : Proof.formula list))
;
(* Rule 8, file Parser.grm, line 47 *)
val _ = update_ yyact 8
(fn () => repr(let
val d__1__ = peekVal 0 : unit
in
([]) end : Proof.proofstep list))
;
(* Rule 9, file Parser.grm, line 48 *)
val _ = update_ yyact 9
(fn () => repr(let
val d__1__ = peekVal 1 : Proof.proofstep
val d__2__ = peekVal 0 : Proof.proofstep list
in
((d__1__) :: (d__2__)) end : Proof.proofstep list))
;
(* Rule 10, file Parser.grm, line 52 *)
val _ = update_ yyact 10
(fn () => repr(let
val d__1__ = peekVal 2 : Proof.rule
val d__2__ = peekVal 1 : Proof.reference list
val d__3__ = peekVal 0 : unit
in
((NONE, (d__1__), (d__2__), "")) end : Proof.proofstep))
;
(* Rule 11, file Parser.grm, line 53 *)
val _ = update_ yyact 11
(fn () => repr(let
val d__1__ = peekVal 2 : Proof.rule
val d__2__ = peekVal 1 : Proof.reference list
val d__3__ = peekVal 0 : Proof.formula * string
in
((SOME (#1 (d__3__)), (d__1__), (d__2__), #2 (d__3__))) end : Proof.proofstep))
;
(* Rule 12, file Parser.grm, line 54 *)
val _ = update_ yyact 12
(fn () => repr(let
val d__1__ = peekVal 3 : Proof.rule
val d__2__ = peekVal 2 : unit
val d__3__ = peekVal 1 : Proof.reference list
val d__4__ = peekVal 0 : Proof.formula * string
in
((SOME (#1 (d__4__)), (d__1__), (d__3__), #2 (d__4__))) end : Proof.proofstep))
;
(* Rule 13, file Parser.grm, line 55 *)
val _ = update_ yyact 13
(fn () => repr(let
val d__1__ = peekVal 1 : Proof.rule
val d__2__ = peekVal 0 : Proof.formula * string
in
((SOME (#1 (d__2__)), (d__1__), [], #2 (d__2__))) end : Proof.proofstep))
;
(* Rule 14, file Parser.grm, line 59 *)
val _ = update_ yyact 14
(fn () => repr(let
val d__1__ = peekVal 5 : unit
val d__2__ = peekVal 4 : Proof.formula
val d__3__ = peekVal 3 : unit
val d__4__ = peekVal 2 : string
val d__5__ = peekVal 1 : unit
val d__6__ = peekVal 0 : unit
in
(((d__2__), (d__4__))) end : Proof.formula * string))
;
(* Rule 15, file Parser.grm, line 60 *)
val _ = update_ yyact 15
(fn () => repr(let
val d__1__ = peekVal 2 : unit
val d__2__ = peekVal 1 : Proof.formula
val d__3__ = peekVal 0 : unit
in
(((d__2__), "")) end : Proof.formula * string))
;
(* Rule 16, file Parser.grm, line 64 *)
val _ = update_ yyact 16
(fn () => repr(let
val d__1__ = peekVal 0 : Proof.reference
in
([(d__1__)]) end : Proof.reference list))
;
(* Rule 17, file Parser.grm, line 65 *)
val _ = update_ yyact 17
(fn () => repr(let
val d__1__ = peekVal 0 : Proof.reference list
in
((d__1__)) end : Proof.reference list))
;
(* Rule 18, file Parser.grm, line 69 *)
val _ = update_ yyact 18
(fn () => repr(let
val d__1__ = peekVal 2 : Proof.reference
val d__2__ = peekVal 1 : unit
val d__3__ = peekVal 0 : Proof.reference
in
((d__1__) :: [(d__3__)]) end : Proof.reference list))
;
(* Rule 19, file Parser.grm, line 70 *)
val _ = update_ yyact 19
(fn () => repr(let
val d__1__ = peekVal 2 : Proof.reference
val d__2__ = peekVal 1 : unit
val d__3__ = peekVal 0 : Proof.reference list
in
((d__1__) :: (d__3__)) end : Proof.reference list))
;
(* Rule 20, file Parser.grm, line 74 *)
val _ = update_ yyact 20
(fn () => repr(let
val d__1__ = peekVal 2 : unit
val d__2__ = peekVal 1 : string
val d__3__ = peekVal 0 : unit
in
(Proof.Line (d__2__)) end : Proof.reference))
;
(* Rule 21, file Parser.grm, line 75 *)
val _ = update_ yyact 21
(fn () => repr(let
val d__1__ = peekVal 10 : unit
val d__2__ = peekVal 9 : string
val d__3__ = peekVal 8 : unit
val d__4__ = peekVal 7 : Proof.reference
val d__5__ = peekVal 6 : unit
val d__6__ = peekVal 5 : string
val d__7__ = peekVal 4 : unit
val d__8__ = peekVal 3 : unit
val d__9__ = peekVal 2 : unit
val d__10__ = peekVal 1 : string
val d__11__ = peekVal 0 : unit
in
(Proof.Box ((d__2__), (d__6__))) end : Proof.reference))
;
(* Rule 22, file Parser.grm, line 78 *)
val _ = update_ yyact 22
(fn () => repr(let
val d__1__ = peekVal 0 : unit
in
(Proof.Cpy) end : Proof.rule))
;
(* Rule 23, file Parser.grm, line 79 *)
val _ = update_ yyact 23
(fn () => repr(let
val d__1__ = peekVal 2 : unit
val d__2__ = peekVal 1 : Proof.rule
val d__3__ = peekVal 0 : unit
in
(Proof.Ain) end : Proof.rule))
;
(* Rule 24, file Parser.grm, line 80 *)
val _ = update_ yyact 24
(fn () => repr(let
val d__1__ = peekVal 4 : unit
val d__2__ = peekVal 3 : Proof.rule
val d__3__ = peekVal 2 : unit
val d__4__ = peekVal 1 : Proof.rule
val d__5__ = peekVal 0 : unit
in
(Proof.Ae1) end : Proof.rule))
;
(* Rule 25, file Parser.grm, line 81 *)
val _ = update_ yyact 25
(fn () => repr(let
val d__1__ = peekVal 6 : unit
val d__2__ = peekVal 5 : Proof.rule
val d__3__ = peekVal 4 : unit
val d__4__ = peekVal 3 : Proof.rule
val d__5__ = peekVal 2 : unit
val d__6__ = peekVal 1 : Proof.rule
val d__7__ = peekVal 0 : unit
in
(Proof.Ae2) end : Proof.rule))
;
(* Rule 26, file Parser.grm, line 82 *)
val _ = update_ yyact 26
(fn () => repr(let
val d__1__ = peekVal 8 : unit
val d__2__ = peekVal 7 : Proof.rule
val d__3__ = peekVal 6 : unit
val d__4__ = peekVal 5 : Proof.rule
val d__5__ = peekVal 4 : unit
val d__6__ = peekVal 3 : Proof.rule
val d__7__ = peekVal 2 : unit
val d__8__ = peekVal 1 : Proof.rule
val d__9__ = peekVal 0 : unit
in
(Proof.Oi1) end : Proof.rule))
;
(* Rule 27, file Parser.grm, line 83 *)
val _ = update_ yyact 27
(fn () => repr(let
val d__1__ = peekVal 10 : unit
val d__2__ = peekVal 9 : Proof.rule
val d__3__ = peekVal 8 : unit
val d__4__ = peekVal 7 : Proof.rule
val d__5__ = peekVal 6 : unit
val d__6__ = peekVal 5 : Proof.rule
val d__7__ = peekVal 4 : unit
val d__8__ = peekVal 3 : Proof.rule
val d__9__ = peekVal 2 : unit
val d__10__ = peekVal 1 : Proof.rule
val d__11__ = peekVal 0 : unit
in
(Proof.Oi2) end : Proof.rule))
;
(* Rule 28, file Parser.grm, line 84 *)
val _ = update_ yyact 28
(fn () => repr(let
val d__1__ = peekVal 12 : unit
val d__2__ = peekVal 11 : Proof.rule
val d__3__ = peekVal 10 : unit
val d__4__ = peekVal 9 : Proof.rule
val d__5__ = peekVal 8 : unit
val d__6__ = peekVal 7 : Proof.rule
val d__7__ = peekVal 6 : unit
val d__8__ = peekVal 5 : Proof.rule
val d__9__ = peekVal 4 : unit
val d__10__ = peekVal 3 : Proof.rule
val d__11__ = peekVal 2 : unit
val d__12__ = peekVal 1 : Proof.rule
val d__13__ = peekVal 0 : unit
in
(Proof.Oel) end : Proof.rule))
;
(* Rule 29, file Parser.grm, line 85 *)
val _ = update_ yyact 29
(fn () => repr(let
val d__1__ = peekVal 14 : unit
val d__2__ = peekVal 13 : Proof.rule
val d__3__ = peekVal 12 : unit
val d__4__ = peekVal 11 : Proof.rule
val d__5__ = peekVal 10 : unit
val d__6__ = peekVal 9 : Proof.rule
val d__7__ = peekVal 8 : unit
val d__8__ = peekVal 7 : Proof.rule
val d__9__ = peekVal 6 : unit
val d__10__ = peekVal 5 : Proof.rule
val d__11__ = peekVal 4 : unit
val d__12__ = peekVal 3 : Proof.rule
val d__13__ = peekVal 2 : unit
val d__14__ = peekVal 1 : Proof.rule
val d__15__ = peekVal 0 : unit
in
(Proof.Iin) end : Proof.rule))
;
(* Rule 30, file Parser.grm, line 86 *)
val _ = update_ yyact 30
(fn () => repr(let
val d__1__ = peekVal 16 : unit
val d__2__ = peekVal 15 : Proof.rule
val d__3__ = peekVal 14 : unit
val d__4__ = peekVal 13 : Proof.rule
val d__5__ = peekVal 12 : unit
val d__6__ = peekVal 11 : Proof.rule
val d__7__ = peekVal 10 : unit
val d__8__ = peekVal 9 : Proof.rule
val d__9__ = peekVal 8 : unit
val d__10__ = peekVal 7 : Proof.rule
val d__11__ = peekVal 6 : unit
val d__12__ = peekVal 5 : Proof.rule
val d__13__ = peekVal 4 : unit
val d__14__ = peekVal 3 : Proof.rule
val d__15__ = peekVal 2 : unit
val d__16__ = peekVal 1 : Proof.rule
val d__17__ = peekVal 0 : unit
in
(Proof.Iel) end : Proof.rule))
;
(* Rule 31, file Parser.grm, line 87 *)
val _ = update_ yyact 31
(fn () => repr(let
val d__1__ = peekVal 18 : unit
val d__2__ = peekVal 17 : Proof.rule
val d__3__ = peekVal 16 : unit
val d__4__ = peekVal 15 : Proof.rule
val d__5__ = peekVal 14 : unit
val d__6__ = peekVal 13 : Proof.rule
val d__7__ = peekVal 12 : unit
val d__8__ = peekVal 11 : Proof.rule
val d__9__ = peekVal 10 : unit
val d__10__ = peekVal 9 : Proof.rule
val d__11__ = peekVal 8 : unit
val d__12__ = peekVal 7 : Proof.rule
val d__13__ = peekVal 6 : unit
val d__14__ = peekVal 5 : Proof.rule
val d__15__ = peekVal 4 : unit
val d__16__ = peekVal 3 : Proof.rule
val d__17__ = peekVal 2 : unit
val d__18__ = peekVal 1 : Proof.rule
val d__19__ = peekVal 0 : unit
in
(Proof.Nin) end : Proof.rule))
;
(* Rule 32, file Parser.grm, line 88 *)
val _ = update_ yyact 32
(fn () => repr(let
val d__1__ = peekVal 20 : unit
val d__2__ = peekVal 19 : Proof.rule
val d__3__ = peekVal 18 : unit
val d__4__ = peekVal 17 : Proof.rule
val d__5__ = peekVal 16 : unit
val d__6__ = peekVal 15 : Proof.rule
val d__7__ = peekVal 14 : unit
val d__8__ = peekVal 13 : Proof.rule
val d__9__ = peekVal 12 : unit
val d__10__ = peekVal 11 : Proof.rule
val d__11__ = peekVal 10 : unit
val d__12__ = peekVal 9 : Proof.rule
val d__13__ = peekVal 8 : unit
val d__14__ = peekVal 7 : Proof.rule
val d__15__ = peekVal 6 : unit
val d__16__ = peekVal 5 : Proof.rule
val d__17__ = peekVal 4 : unit
val d__18__ = peekVal 3 : Proof.rule
val d__19__ = peekVal 2 : unit
val d__20__ = peekVal 1 : Proof.rule
val d__21__ = peekVal 0 : unit
in
(Proof.Nel) end : Proof.rule))
;
(* Rule 33, file Parser.grm, line 89 *)
val _ = update_ yyact 33
(fn () => repr(let
val d__1__ = peekVal 22 : unit
val d__2__ = peekVal 21 : Proof.rule
val d__3__ = peekVal 20 : unit
val d__4__ = peekVal 19 : Proof.rule
val d__5__ = peekVal 18 : unit
val d__6__ = peekVal 17 : Proof.rule
val d__7__ = peekVal 16 : unit
val d__8__ = peekVal 15 : Proof.rule
val d__9__ = peekVal 14 : unit
val d__10__ = peekVal 13 : Proof.rule
val d__11__ = peekVal 12 : unit
val d__12__ = peekVal 11 : Proof.rule
val d__13__ = peekVal 10 : unit
val d__14__ = peekVal 9 : Proof.rule
val d__15__ = peekVal 8 : unit
val d__16__ = peekVal 7 : Proof.rule
val d__17__ = peekVal 6 : unit
val d__18__ = peekVal 5 : Proof.rule
val d__19__ = peekVal 4 : unit
val d__20__ = peekVal 3 : Proof.rule
val d__21__ = peekVal 2 : unit
val d__22__ = peekVal 1 : Proof.rule
val d__23__ = peekVal 0 : unit
in
(Proof.Din) end : Proof.rule))
;
(* Rule 34, file Parser.grm, line 90 *)
val _ = update_ yyact 34
(fn () => repr(let
val d__1__ = peekVal 24 : unit
val d__2__ = peekVal 23 : Proof.rule
val d__3__ = peekVal 22 : unit
val d__4__ = peekVal 21 : Proof.rule
val d__5__ = peekVal 20 : unit
val d__6__ = peekVal 19 : Proof.rule
val d__7__ = peekVal 18 : unit
val d__8__ = peekVal 17 : Proof.rule
val d__9__ = peekVal 16 : unit
val d__10__ = peekVal 15 : Proof.rule
val d__11__ = peekVal 14 : unit
val d__12__ = peekVal 13 : Proof.rule
val d__13__ = peekVal 12 : unit
val d__14__ = peekVal 11 : Proof.rule
val d__15__ = peekVal 10 : unit
val d__16__ = peekVal 9 : Proof.rule
val d__17__ = peekVal 8 : unit
val d__18__ = peekVal 7 : Proof.rule
val d__19__ = peekVal 6 : unit
val d__20__ = peekVal 5 : Proof.rule
val d__21__ = peekVal 4 : unit
val d__22__ = peekVal 3 : Proof.rule
val d__23__ = peekVal 2 : unit
val d__24__ = peekVal 1 : Proof.rule
val d__25__ = peekVal 0 : unit
in
(Proof.Del) end : Proof.rule))
;
(* Rule 35, file Parser.grm, line 91 *)
val _ = update_ yyact 35
(fn () => repr(let
val d__1__ = peekVal 26 : unit
val d__2__ = peekVal 25 : Proof.rule
val d__3__ = peekVal 24 : unit
val d__4__ = peekVal 23 : Proof.rule
val d__5__ = peekVal 22 : unit
val d__6__ = peekVal 21 : Proof.rule
val d__7__ = peekVal 20 : unit
val d__8__ = peekVal 19 : Proof.rule
val d__9__ = peekVal 18 : unit
val d__10__ = peekVal 17 : Proof.rule
val d__11__ = peekVal 16 : unit
val d__12__ = peekVal 15 : Proof.rule
val d__13__ = peekVal 14 : unit
val d__14__ = peekVal 13 : Proof.rule
val d__15__ = peekVal 12 : unit
val d__16__ = peekVal 11 : Proof.rule
val d__17__ = peekVal 10 : unit
val d__18__ = peekVal 9 : Proof.rule
val d__19__ = peekVal 8 : unit
val d__20__ = peekVal 7 : Proof.rule
val d__21__ = peekVal 6 : unit
val d__22__ = peekVal 5 : Proof.rule
val d__23__ = peekVal 4 : unit
val d__24__ = peekVal 3 : Proof.rule
val d__25__ = peekVal 2 : unit
val d__26__ = peekVal 1 : Proof.rule
val d__27__ = peekVal 0 : unit
in
(Proof.Bel) end : Proof.rule))
;
(* Rule 36, file Parser.grm, line 92 *)
val _ = update_ yyact 36
(fn () => repr(let
val d__1__ = peekVal 28 : unit
val d__2__ = peekVal 27 : Proof.rule
val d__3__ = peekVal 26 : unit
val d__4__ = peekVal 25 : Proof.rule
val d__5__ = peekVal 24 : unit
val d__6__ = peekVal 23 : Proof.rule
val d__7__ = peekVal 22 : unit
val d__8__ = peekVal 21 : Proof.rule
val d__9__ = peekVal 20 : unit
val d__10__ = peekVal 19 : Proof.rule
val d__11__ = peekVal 18 : unit
val d__12__ = peekVal 17 : Proof.rule
val d__13__ = peekVal 16 : unit
val d__14__ = peekVal 15 : Proof.rule
val d__15__ = peekVal 14 : unit
val d__16__ = peekVal 13 : Proof.rule
val d__17__ = peekVal 12 : unit
val d__18__ = peekVal 11 : Proof.rule
val d__19__ = peekVal 10 : unit
val d__20__ = peekVal 9 : Proof.rule
val d__21__ = peekVal 8 : unit
val d__22__ = peekVal 7 : Proof.rule
val d__23__ = peekVal 6 : unit
val d__24__ = peekVal 5 : Proof.rule
val d__25__ = peekVal 4 : unit
val d__26__ = peekVal 3 : Proof.rule
val d__27__ = peekVal 2 : unit
val d__28__ = peekVal 1 : Proof.rule
val d__29__ = peekVal 0 : unit
in
(Proof.Mod) end : Proof.rule))
;
(* Rule 37, file Parser.grm, line 93 *)
val _ = update_ yyact 37
(fn () => repr(let
val d__1__ = peekVal 30 : unit
val d__2__ = peekVal 29 : Proof.rule
val d__3__ = peekVal 28 : unit
val d__4__ = peekVal 27 : Proof.rule
val d__5__ = peekVal 26 : unit
val d__6__ = peekVal 25 : Proof.rule
val d__7__ = peekVal 24 : unit
val d__8__ = peekVal 23 : Proof.rule
val d__9__ = peekVal 22 : unit
val d__10__ = peekVal 21 : Proof.rule
val d__11__ = peekVal 20 : unit
val d__12__ = peekVal 19 : Proof.rule
val d__13__ = peekVal 18 : unit
val d__14__ = peekVal 17 : Proof.rule
val d__15__ = peekVal 16 : unit
val d__16__ = peekVal 15 : Proof.rule
val d__17__ = peekVal 14 : unit
val d__18__ = peekVal 13 : Proof.rule
val d__19__ = peekVal 12 : unit
val d__20__ = peekVal 11 : Proof.rule
val d__21__ = peekVal 10 : unit
val d__22__ = peekVal 9 : Proof.rule
val d__23__ = peekVal 8 : unit
val d__24__ = peekVal 7 : Proof.rule
val d__25__ = peekVal 6 : unit
val d__26__ = peekVal 5 : Proof.rule
val d__27__ = peekVal 4 : unit
val d__28__ = peekVal 3 : Proof.rule
val d__29__ = peekVal 2 : unit
val d__30__ = peekVal 1 : Proof.rule
val d__31__ = peekVal 0 : unit
in
(Proof.Pbc) end : Proof.rule))
;
(* Rule 38, file Parser.grm, line 94 *)
val _ = update_ yyact 38
(fn () => repr(let
val d__1__ = peekVal 32 : unit
val d__2__ = peekVal 31 : Proof.rule
val d__3__ = peekVal 30 : unit
val d__4__ = peekVal 29 : Proof.rule
val d__5__ = peekVal 28 : unit
val d__6__ = peekVal 27 : Proof.rule
val d__7__ = peekVal 26 : unit
val d__8__ = peekVal 25 : Proof.rule
val d__9__ = peekVal 24 : unit
val d__10__ = peekVal 23 : Proof.rule
val d__11__ = peekVal 22 : unit
val d__12__ = peekVal 21 : Proof.rule
val d__13__ = peekVal 20 : unit
val d__14__ = peekVal 19 : Proof.rule
val d__15__ = peekVal 18 : unit
val d__16__ = peekVal 17 : Proof.rule
val d__17__ = peekVal 16 : unit
val d__18__ = peekVal 15 : Proof.rule
val d__19__ = peekVal 14 : unit
val d__20__ = peekVal 13 : Proof.rule
val d__21__ = peekVal 12 : unit
val d__22__ = peekVal 11 : Proof.rule
val d__23__ = peekVal 10 : unit
val d__24__ = peekVal 9 : Proof.rule
val d__25__ = peekVal 8 : unit
val d__26__ = peekVal 7 : Proof.rule
val d__27__ = peekVal 6 : unit
val d__28__ = peekVal 5 : Proof.rule
val d__29__ = peekVal 4 : unit
val d__30__ = peekVal 3 : Proof.rule
val d__31__ = peekVal 2 : unit
val d__32__ = peekVal 1 : Proof.rule
val d__33__ = peekVal 0 : unit
in
(Proof.Lem) end : Proof.rule))
;
(* Rule 39, file Parser.grm, line 98 *)
val _ = update_ yyact 39
(fn () => repr(let
val d__1__ = peekVal 0 : unit
in
(Proof.BOT) end : Proof.formula))
;
(* Rule 40, file Parser.grm, line 99 *)
val _ = update_ yyact 40
(fn () => repr(let
val d__1__ = peekVal 0 : string
in
(Proof.Atom (d__1__)) end : Proof.formula))
;
(* Rule 41, file Parser.grm, line 100 *)
val _ = update_ yyact 41
(fn () => repr(let
val d__1__ = peekVal 2 : Proof.formula
val d__2__ = peekVal 1 : unit
val d__3__ = peekVal 0 : Proof.formula
in
(Proof.IMP ((d__1__), (d__3__))) end : Proof.formula))
;
(* Rule 42, file Parser.grm, line 101 *)
val _ = update_ yyact 42
(fn () => repr(let
val d__1__ = peekVal 2 : Proof.formula
val d__2__ = peekVal 1 : unit
val d__3__ = peekVal 0 : Proof.formula
in
(Proof.OR  ((d__1__), (d__3__))) end : Proof.formula))
;
(* Rule 43, file Parser.grm, line 102 *)
val _ = update_ yyact 43
(fn () => repr(let
val d__1__ = peekVal 2 : Proof.formula
val d__2__ = peekVal 1 : unit
val d__3__ = peekVal 0 : Proof.formula
in
(Proof.AND ((d__1__), (d__3__))) end : Proof.formula))
;
(* Rule 44, file Parser.grm, line 103 *)
val _ = update_ yyact 44
(fn () => repr(let
val d__1__ = peekVal 1 : unit
val d__2__ = peekVal 0 : Proof.formula
in
(Proof.NEG (d__2__)) end : Proof.formula))
;
(* Rule 45, file Parser.grm, line 104 *)
val _ = update_ yyact 45
(fn () => repr(let
val d__1__ = peekVal 2 : unit
val d__2__ = peekVal 1 : Proof.formula
val d__3__ = peekVal 0 : unit
in
((d__2__)) end : Proof.formula))
;
(* Entry Proof *)
val _ = update_ yyact 46 (fn () => raise yyexit (peekVal 0));
val yytables : parseTables =
  ( yyact,
    yytransl,
    yylhs,
    yylen,
    yydefred,
    yydgoto,
    yysindex,
    yyrindex,
    yygindex,
    YYTABLESIZE,
    yytable,
    yycheck );
fun Proof lexer lexbuf = yyparse yytables 1 lexer lexbuf;
