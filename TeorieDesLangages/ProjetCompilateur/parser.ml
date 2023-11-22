type token =
  | EOF
  | DIMENSIONS
  | END
  | OF
  | ASSIGN
  | COMMA
  | LBRACKET
  | RBRACKET
  | DOT_DOT
  | DOT
  | ADD
  | SUB
  | LPARENT
  | RPARENT
  | MUL
  | DIV
  | MOD
  | IF
  | ELSE
  | ELSIF
  | THEN
  | WHEN
  | OTHERWISE
  | EQUAL
  | DIFF
  | INF
  | SUPP
  | INFEQ
  | SUPPEQ
  | ID of (string)
  | INT of (int)

open Parsing;;
let _ = parse_error;;
# 17 "parser.mly"

open Common
open Ast
open Printf
open Symbols

(** Raise a syntax error with the given message.
	@param msg	Message of the error. *)
let error msg =
	raise (SyntaxError msg)


(** Restructure the when assignment into selections.
	@param f	Function to build the assignment.
	@param v	Initial values.
	@param ws	Sequence of (condition, expression).
	@return		Built statement. *)
let rec make_when f v ws =
	match ws with
	| [] ->	f v
	| (c, nv)::t ->
		IF_THEN(c, f v, make_when f nv t)

# 61 "parser.ml"
let yytransl_const = [|
    0 (* EOF *);
  257 (* DIMENSIONS *);
  258 (* END *);
  259 (* OF *);
  260 (* ASSIGN *);
  261 (* COMMA *);
  262 (* LBRACKET *);
  263 (* RBRACKET *);
  264 (* DOT_DOT *);
  265 (* DOT *);
  266 (* ADD *);
  267 (* SUB *);
  268 (* LPARENT *);
  269 (* RPARENT *);
  270 (* MUL *);
  271 (* DIV *);
  272 (* MOD *);
  273 (* IF *);
  274 (* ELSE *);
  275 (* ELSIF *);
  276 (* THEN *);
  277 (* WHEN *);
  278 (* OTHERWISE *);
  279 (* EQUAL *);
  280 (* DIFF *);
  281 (* INF *);
  282 (* SUPP *);
  283 (* INFEQ *);
  284 (* SUPPEQ *);
    0|]

let yytransl_block = [|
  285 (* ID *);
  286 (* INT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\004\000\004\000\005\000\003\000\003\000\
\006\000\006\000\006\000\006\000\006\000\009\000\009\000\011\000\
\011\000\011\000\010\000\010\000\010\000\010\000\010\000\010\000\
\007\000\012\000\012\000\012\000\012\000\012\000\012\000\008\000\
\008\000\008\000\013\000\013\000\013\000\013\000\000\000"

let yylen = "\002\000\
\007\000\003\000\001\000\001\000\003\000\005\000\000\000\002\000\
\003\000\003\000\003\000\003\000\006\000\002\000\005\000\000\000\
\005\000\002\000\003\000\003\000\003\000\003\000\003\000\003\000\
\005\000\001\000\001\000\001\000\002\000\002\000\003\000\003\000\
\003\000\001\000\001\000\003\000\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\039\000\000\000\000\000\000\000\000\000\
\000\000\000\000\004\000\000\000\000\000\000\000\000\000\000\000\
\002\000\000\000\000\000\000\000\000\000\000\000\000\000\005\000\
\000\000\000\000\000\000\000\000\000\000\028\000\027\000\026\000\
\000\000\000\000\035\000\000\000\000\000\001\000\008\000\000\000\
\006\000\000\000\029\000\030\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\011\000\000\000\012\000\000\000\031\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\036\000\037\000\038\000\000\000\014\000\025\000\000\000\000\000\
\000\000\000\000\018\000\000\000\013\000\000\000\000\000\000\000\
\015\000\000\000\017\000"

let yydgoto = "\002\000\
\004\000\009\000\021\000\010\000\011\000\022\000\032\000\033\000\
\059\000\034\000\081\000\035\000\036\000"

let yysindex = "\005\000\
\237\254\000\000\018\255\000\000\010\255\057\255\033\255\058\255\
\066\255\037\255\000\000\044\255\051\255\001\255\055\255\087\255\
\000\000\067\255\040\255\092\255\098\000\001\255\095\255\000\000\
\070\255\096\255\040\255\040\255\040\255\000\000\000\000\000\000\
\052\255\082\255\000\000\039\255\040\255\000\000\000\000\040\255\
\000\000\074\255\000\000\000\000\034\255\040\255\040\255\040\255\
\040\255\040\255\040\255\040\255\040\255\001\255\040\255\040\255\
\040\255\061\255\000\000\061\255\000\000\098\255\000\000\039\255\
\039\255\079\255\079\255\079\255\079\255\079\255\079\255\073\255\
\000\000\000\000\000\000\040\255\000\000\000\000\001\255\040\255\
\101\255\102\255\000\000\086\255\000\000\040\255\001\255\061\255\
\000\000\073\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\106\255\000\000\000\000\000\000\109\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\014\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\003\255\000\000\000\000\
\000\000\085\000\000\000\088\000\000\000\000\000\000\000\029\000\
\057\000\011\255\012\255\015\255\028\255\036\255\038\255\108\255\
\000\000\000\000\000\000\000\000\000\000\000\000\109\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\003\255\000\000\
\000\000\108\255\000\000"

let yygindex = "\000\000\
\000\000\000\000\236\255\000\000\097\000\000\000\242\255\231\255\
\219\255\240\255\023\000\238\255\047\000"

let yytablesize = 373
let yytable = "\023\000\
\034\000\039\000\061\000\045\000\007\000\001\000\018\000\023\000\
\043\000\044\000\003\000\058\000\006\000\007\000\060\000\019\000\
\020\000\019\000\005\000\021\000\007\000\007\000\066\000\067\000\
\068\000\069\000\070\000\071\000\032\000\020\000\019\000\020\000\
\022\000\072\000\021\000\012\000\073\000\074\000\075\000\023\000\
\023\000\015\000\024\000\046\000\047\000\018\000\063\000\022\000\
\089\000\027\000\028\000\029\000\055\000\056\000\057\000\023\000\
\033\000\024\000\083\000\082\000\088\000\046\000\047\000\084\000\
\023\000\013\000\090\000\014\000\030\000\031\000\046\000\047\000\
\023\000\016\000\048\000\049\000\050\000\051\000\052\000\053\000\
\017\000\076\000\077\000\007\000\010\000\007\000\008\000\009\000\
\046\000\047\000\079\000\080\000\064\000\065\000\025\000\037\000\
\026\000\038\000\040\000\041\000\042\000\054\000\085\000\062\000\
\078\000\087\000\086\000\003\000\007\000\016\000\007\000\024\000\
\091\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\034\000\000\000\000\000\034\000\034\000\000\000\
\000\000\000\000\034\000\034\000\000\000\034\000\000\000\007\000\
\000\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
\034\000\034\000\034\000\034\000\034\000\034\000\032\000\007\000\
\007\000\032\000\032\000\000\000\000\000\000\000\032\000\032\000\
\000\000\032\000\000\000\000\000\000\000\032\000\032\000\032\000\
\032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
\032\000\032\000\033\000\000\000\000\000\033\000\033\000\000\000\
\000\000\000\000\033\000\033\000\000\000\033\000\000\000\000\000\
\000\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
\033\000\033\000\033\000\033\000\033\000\033\000\010\000\000\000\
\000\000\009\000\010\000\000\000\000\000\009\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\010\000\010\000\010\000\
\009\000\009\000\009\000\000\000\000\000\000\000\000\000\000\000\
\000\000\010\000\000\000\000\000\009\000"

let yycheck = "\014\000\
\000\000\022\000\040\000\029\000\002\001\001\000\006\001\022\000\
\027\000\028\000\030\001\037\000\003\001\000\000\040\000\005\001\
\005\001\017\001\001\001\005\001\018\001\019\001\048\000\049\000\
\050\000\051\000\052\000\053\000\000\000\029\001\020\001\020\001\
\005\001\054\000\020\001\003\001\055\000\056\000\057\000\054\000\
\005\001\005\001\005\001\010\001\011\001\006\001\013\001\020\001\
\086\000\010\001\011\001\012\001\014\001\015\001\016\001\020\001\
\000\000\020\001\079\000\076\000\086\000\010\001\011\001\080\000\
\079\000\008\001\087\000\002\001\029\001\030\001\010\001\011\001\
\087\000\030\001\023\001\024\001\025\001\026\001\027\001\028\001\
\030\001\021\001\022\001\029\001\000\000\029\001\030\001\000\000\
\010\001\011\001\018\001\019\001\046\000\047\000\008\001\004\001\
\030\001\000\000\004\001\030\001\005\001\020\001\002\001\030\001\
\007\001\020\001\005\001\002\001\000\000\002\001\002\001\015\000\
\090\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\002\001\255\255\255\255\005\001\006\001\255\255\
\255\255\255\255\010\001\011\001\255\255\013\001\255\255\002\001\
\255\255\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\028\001\029\001\002\001\018\001\
\019\001\005\001\006\001\255\255\255\255\255\255\010\001\011\001\
\255\255\013\001\255\255\255\255\255\255\017\001\018\001\019\001\
\020\001\021\001\022\001\023\001\024\001\025\001\026\001\027\001\
\028\001\029\001\002\001\255\255\255\255\005\001\006\001\255\255\
\255\255\255\255\010\001\011\001\255\255\013\001\255\255\255\255\
\255\255\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\028\001\029\001\002\001\255\255\
\255\255\002\001\006\001\255\255\255\255\006\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\017\001\018\001\019\001\
\017\001\018\001\019\001\255\255\255\255\255\255\255\255\255\255\
\255\255\029\001\255\255\255\255\029\001"

let yynames_const = "\
  EOF\000\
  DIMENSIONS\000\
  END\000\
  OF\000\
  ASSIGN\000\
  COMMA\000\
  LBRACKET\000\
  RBRACKET\000\
  DOT_DOT\000\
  DOT\000\
  ADD\000\
  SUB\000\
  LPARENT\000\
  RPARENT\000\
  MUL\000\
  DIV\000\
  MOD\000\
  IF\000\
  ELSE\000\
  ELSIF\000\
  THEN\000\
  WHEN\000\
  OTHERWISE\000\
  EQUAL\000\
  DIFF\000\
  INF\000\
  SUPP\000\
  INFEQ\000\
  SUPPEQ\000\
  "

let yynames_block = "\
  ID\000\
  INT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'config) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'opt_statements) in
    Obj.repr(
# 86 "parser.mly"
 (
		if _1 != 2 then error "only 2 dimension accepted";
		(_4, _6)
	)
# 311 "parser.ml"
               : Ast.prog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 94 "parser.mly"
  (
			if _1 >= _3 then error "illegal field values";
			[("", (0, (_1, _3)))]
		)
# 322 "parser.ml"
               : 'config))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fields) in
    Obj.repr(
# 99 "parser.mly"
  ( set_fields _1 )
# 329 "parser.ml"
               : 'config))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'field) in
    Obj.repr(
# 104 "parser.mly"
  ( [_1] )
# 336 "parser.ml"
               : 'fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'fields) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'field) in
    Obj.repr(
# 106 "parser.mly"
  (_3 :: _1 )
# 344 "parser.ml"
               : 'fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 111 "parser.mly"
  (
			if _3 >= _5 then error "illegal field values";
			(_1, (_3, _5))
		)
# 356 "parser.ml"
               : 'field))
; (fun __caml_parser_env ->
    Obj.repr(
# 119 "parser.mly"
  ( NOP )
# 362 "parser.ml"
               : 'opt_statements))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'statement) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'opt_statements) in
    Obj.repr(
# 121 "parser.mly"
    ( SEQ (_1, _2) )
# 370 "parser.ml"
               : 'opt_statements))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'cell) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expressions) in
    Obj.repr(
# 127 "parser.mly"
  (
			if (fst _1) != 0 then error "assigned x must be 0";
			if (snd _1) != 0 then error "assigned Y must be 0";
			SET_CELL (0, _3)
		)
# 382 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expressions) in
    Obj.repr(
# 134 "parser.mly"
    ( 
		if (get_var _1) = -1 then SET_VAR(declare_var(_1),_3) else SET_VAR(get_var(_1),_3);
	)
# 392 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'when_clause) in
    Obj.repr(
# 139 "parser.mly"
 ( NOP )
# 400 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'cell) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'when_clause) in
    Obj.repr(
# 141 "parser.mly"
 ( NOP )
# 408 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'condition) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'opt_statements) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'else_clause) in
    Obj.repr(
# 144 "parser.mly"
 ( IF_THEN(_2,_4,_5) )
# 417 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expressions) in
    Obj.repr(
# 150 "parser.mly"
 ( NONE )
# 424 "parser.ml"
               : 'when_clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'expressions) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'condition) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'when_clause) in
    Obj.repr(
# 152 "parser.mly"
   ( NONE )
# 433 "parser.ml"
               : 'when_clause))
; (fun __caml_parser_env ->
    Obj.repr(
# 156 "parser.mly"
        ( NOP )
# 439 "parser.ml"
               : 'else_clause))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'condition) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'opt_statements) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'else_clause) in
    Obj.repr(
# 158 "parser.mly"
        (
            IF_THEN(_2, _4, _5)
        )
# 450 "parser.ml"
               : 'else_clause))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'opt_statements) in
    Obj.repr(
# 162 "parser.mly"
        ( _2 )
# 457 "parser.ml"
               : 'else_clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expressions) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expressions) in
    Obj.repr(
# 167 "parser.mly"
  ( COMP(COMP_EQ, _1, _3) )
# 465 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expressions) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expressions) in
    Obj.repr(
# 169 "parser.mly"
  ( COMP(COMP_NE, _1, _3) )
# 473 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expressions) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expressions) in
    Obj.repr(
# 171 "parser.mly"
  ( COMP(COMP_LT, _1, _3) )
# 481 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expressions) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expressions) in
    Obj.repr(
# 173 "parser.mly"
  ( COMP(COMP_GT, _1, _3) )
# 489 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expressions) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expressions) in
    Obj.repr(
# 175 "parser.mly"
  ( COMP(COMP_LE, _1, _3) )
# 497 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expressions) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expressions) in
    Obj.repr(
# 177 "parser.mly"
  ( COMP(COMP_GE, _1, _3) )
# 505 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 181 "parser.mly"
  (
			if (_2 < -1) || (_2 > 1) then error "x out of range";
			if (_4 < -1) || (_4 > 1) then error "x out of range";
			(_2, _4)
		)
# 517 "parser.ml"
               : 'cell))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'cell) in
    Obj.repr(
# 190 "parser.mly"
  ( CELL (0, fst _1, snd _1) )
# 524 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 192 "parser.mly"
  ( CST _1 )
# 531 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 194 "parser.mly"
  ( VAR(get_var(_1)) )
# 538 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 196 "parser.mly"
     ( _2 )
# 545 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 198 "parser.mly"
     ( NEG(_2) )
# 552 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expressions) in
    Obj.repr(
# 200 "parser.mly"
     ( _2 )
# 559 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expressions) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression_mul_div_mod) in
    Obj.repr(
# 205 "parser.mly"
    (
      BINOP( OP_ADD, _1, _3)
	)
# 569 "parser.ml"
               : 'expressions))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expressions) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression_mul_div_mod) in
    Obj.repr(
# 209 "parser.mly"
    ( 
		BINOP( OP_SUB, _1, _3)
	)
# 579 "parser.ml"
               : 'expressions))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expression_mul_div_mod) in
    Obj.repr(
# 213 "parser.mly"
    ( _1 )
# 586 "parser.ml"
               : 'expressions))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 218 "parser.mly"
    ( _1 )
# 593 "parser.ml"
               : 'expression_mul_div_mod))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression_mul_div_mod) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 221 "parser.mly"
   ( 
		BINOP( OP_MUL, _1, _3)
	)
# 603 "parser.ml"
               : 'expression_mul_div_mod))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression_mul_div_mod) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 226 "parser.mly"
   ( 
		BINOP( OP_DIV, _1, _3)
	)
# 613 "parser.ml"
               : 'expression_mul_div_mod))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression_mul_div_mod) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 231 "parser.mly"
   ( 
		BINOP( OP_MOD, _1, _3)
	)
# 623 "parser.ml"
               : 'expression_mul_div_mod))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.prog)
