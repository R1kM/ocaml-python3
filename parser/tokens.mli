
(* The type of tokens. *)

type token = 
  | YIELD
  | WITH
  | WHILE
  | TRY
  | TRUE
  | TDIVEQ
  | TDIV
  | SUBEQ
  | SUB
  | STR of (string)
  | SEMICOLEND
  | SEMICOL
  | RSQ
  | RSHIFTEQ
  | RSHIFT
  | RPAR
  | RETURN
  | RBRACE
  | RAISE
  | POWEQ
  | POW
  | PASS
  | OR
  | NOT
  | NONLOCAL
  | NONE
  | NEWLINE
  | NEQ
  | MULEQ
  | MUL
  | MODEQ
  | MOD
  | LT
  | LSQ
  | LSHIFTEQ
  | LSHIFT
  | LPAR
  | LE
  | LBRACE
  | LAMBDA
  | IS
  | INT of (int)
  | INDENT
  | IN
  | IMPORT
  | IMAG of (string)
  | IF
  | IDENT of (string)
  | GT
  | GLOBAL
  | GE
  | FROM
  | FOR
  | FLOAT of (float)
  | FINALLY
  | FALSE
  | EXCEPT
  | EQUAL
  | EQ
  | EOF
  | ELSE
  | ELIF
  | DOT
  | DIVEQ
  | DIV
  | DEL
  | DEF
  | DEDENT
  | CONTINUE
  | COMMARSQ
  | COMMARPAR
  | COMMARBRA
  | COMMA
  | COLON
  | CLASS
  | BYTES of (string)
  | BREAK
  | BITXOREQ
  | BITXOR
  | BITOREQ
  | BITOR
  | BITNOT
  | BITANDEQ
  | BITAND
  | AWAIT
  | ATEQ
  | AT
  | ASYNC
  | ASSERT
  | AS
  | ARROW
  | AND
  | ADDEQ
  | ADD
