%{
    open Format
    open Cst

    let breakClassdef = function
        | ClassDef (name, bases, keywords, body, decs) -> name, bases, keywords, body, decs
        | _ -> assert false

    let breakFunctiondef = function
        | FunctionDef (name, args, body, decs, ret) -> name, args, body, decs, ret
        | _ -> assert false

    let breakAsyncFundef = function
        | AsyncFunctionDef (name, args, body, decs, ret) -> name, args, body, decs, ret
        | _ -> assert false

    let breakFor = function
        | For (t, it, b, e) -> t, it, b, e
        | _ -> assert false

    let breakWith = function
        | With (it, b) -> it, b
        | _ -> assert false
%}

%token <string> IDENT
%token <int> INT
%token <float> FLOAT
%token <string> IMAG
%token <string> STR
%token <string> BYTES

%token INDENT DEDENT NEWLINE
%token EOF

(* Operators *)
%token ADD SUB MUL POW DIV TDIV MOD AT
%token LSHIFT RSHIFT BITAND BITOR BITXOR BITNOT
%token LT GT LE GE EQUAL NEQ

%token SEMICOLEND (* ; at the end of a line *)
%token COMMARSQ   (* , ] *)
%token COMMARPAR  (* , ) *)
%token COMMARBRA  (* , } *)

(* Delimiters *)
%token LPAR RPAR LSQ RSQ LBRACE RBRACE
%token COMMA COLON DOT SEMICOL EQ ARROW
%token ADDEQ SUBEQ MULEQ DIVEQ TDIVEQ MODEQ ATEQ
%token BITANDEQ BITOREQ BITXOREQ RSHIFTEQ LSHIFTEQ POWEQ

(* List of tokens for keywords *)

%token FALSE NONE TRUE AND AS ASSERT BREAK
%token CLASS CONTINUE DEF DEL ELIF ELSE EXCEPT
%token FINALLY FOR FROM GLOBAL IF IMPORT IN
%token IS LAMBDA NONLOCAL NOT OR PASS RAISE
%token RETURN TRY WHILE WITH YIELD

(* Soon to be keywords, so we'll consider as such *)
%token AWAIT ASYNC

/* Entrypoint */
%start file_input

/* Type returned */
%type <Cst.modl> file_input

%type <Cst.stmt list> stmt
%type <string> dotted_name

%%

/* We omit single_input and eval_input */

file_input:
    nl_stmt_list EOF { Module ($1) }
;

nl_stmt_list:
    | { [] }
    | NEWLINE nl_stmt_list { $2 }
    | stmt nl_stmt_list { $1 @ $2 }
;

decorator:
    | AT dotted_name NEWLINE { Name ($2, Load) }
    | AT dotted_name LPAR RPAR NEWLINE { Call (Name($2, Load), [], []) }
    | AT dotted_name LPAR arglist RPAR NEWLINE
        { Call (Name($2, Load), fst $4, snd $4) }
    | AT dotted_name LPAR arglist COMMARPAR NEWLINE 
        { Call (Name($2, Load), fst $4, snd $4) }
;

decorators:
    decorator+  { $1 }
;

decorated:
    | decorators classdef  
         { let (name, bases, keywords, body, _) = breakClassdef $2 in ClassDef(name, bases, keywords, body, $1) }
    | decorators funcdef   
         { let (name, bases, body, _, ret) = breakFunctiondef $2 in FunctionDef(name, bases, body, $1, ret) }
    | decorators async_funcdef 
         { let (name, bases, body, _, ret) = breakAsyncFundef $2 in AsyncFunctionDef(name, bases, body, $1, ret) }
 ;
        
async_funcdef:
    ASYNC funcdef   { let (names, bases, body, dec_l, ret) = breakFunctiondef $2 in AsyncFunctionDef(names, bases, body, dec_l, ret) }
;

funcdef:
    | DEF name parameters COLON suite 
        { FunctionDef($2, $3, $5, [], None) }
    | DEF name parameters ARROW test COLON suite 
        { FunctionDef($2, $3, $7, [], Some $5) }
;

parameters:
    | LPAR typedargslist RPAR  { $2 }
    | LPAR typedargslist COMMARPAR  { $2 }
;

typedargslist:
   | { [], (None : arg option), [], [], (None : arg option), [] }
   | tfpdef { [$1], (None : arg option), [], [], (None : arg option), [Null] }
   | tfpdef EQ test { [$1], (None : arg option), [], [], (None : arg option), [$3] }
   | tfpdef COMMA typedargslist {
       match $3 with (a, va, kwon, kwdef, kwa, def) ->
       ($1 :: a, va, kwon, kwdef, kwa, Null :: def)
       }   
   | tfpdef EQ test COMMA typedargslist {
       match $5 with (a, va, kwon, kwdef, kwa, def) ->
       ($1 :: a, va, kwon, kwdef, kwa, $3 :: def)
    }
   | MUL tfpvarargs { match $2 with (va, kwon, kwdef, kwa) ->
        ( [], va, kwon, kwdef, kwa, [] ) }
   | POW tfpdef     { ([], (None : arg option), [], [], Some $2, []) }
;

tfpdef:
    | name { ($1, (None : expr option)) }
    | name COLON test { ($1, Some $3) }
;

tfpvarargs:
    | tfpdef tfpkwonly_args { 
        match $2 with (kwonly, kwdef, kwargs) ->
        (Some $1, kwonly, kwdef, kwargs) }
    | COMMA tfpdef tfpkwonly_args {
            match $3 with (kwonly, kwdef, kwarg) ->
            ((None : arg option), $2 :: kwonly, Null :: kwdef, kwarg)
        }
    | COMMA tfpdef EQ test tfpkwonly_args {
        match $5 with (kwonly, kwdef, kwarg) ->
            ((None : arg option), $2 :: kwonly, $4 :: kwdef, kwarg)
        }
;        

tfpkwonly_args:
    | COMMA tfpdef tfpkwonly_args { match $3 with
            ( kwonly, kwdef, kwarg) ->
            ($2 :: kwonly, Null :: kwdef, kwarg)
        }
    | COMMA tfpdef EQ test tfpkwonly_args {
        match $5 with (kwonly, kwdef, kwarg) ->
            ($2 :: kwonly, $4 :: kwdef, kwarg)
        }
    | COMMA tfpkwargs { ([], [], Some $2) }
    | { [], [], (None : arg option) }
;

tfpkwargs:
    POW tfpdef { $2 }
;

/* varargs is similar to typedargslist, without the possible annotations */
varargslist:
   | { [], None, [], [], None, [] }
   | vfpdef { [$1], None, [], [], None, [Null] }
   | vfpdef EQ test { [$1], None, [], [], None, [$3] }
   | vfpdef COMMA varargslist {
       match $3 with (a, va, kwon, kwdef, kwa, def) ->
       ($1 :: a, va, kwon, kwdef, kwa, Null :: def)
       }   
   | vfpdef EQ test COMMA varargslist {
       match $5 with (a, va, kwon, kwdef, kwa, def) ->
       ($1 :: a, va, kwon, kwdef, kwa, $3 :: def)
    }
   | vfpvarargs { match $1 with (va, kwon, kwdef, kwa) ->
        ( [], va, kwon, kwdef, kwa, [] ) }
   | POW vfpdef     { ([], (None : arg option), [], [], Some $2, []) }
;

vfpdef:
    | name { ($1, (None : expr option)) }
;

vfpvarargs:
    | MUL vfpdef vfpkwonly_args { 
        match $3 with (kwonly, kwdef, kwargs) ->
        (Some $2, kwonly, kwdef, kwargs) }
    | MUL COMMA vfpdef vfpkwonly_args {
            match $4 with (kwonly, kwdef, kwarg) ->
            (None, $3 :: kwonly, Null :: kwdef, kwarg)
        }
;        

vfpkwonly_args:
    | COMMA vfpdef vfpkwonly_args { match $3 with
            ( kwonly, kwdef, kwarg) ->
            ($2 :: kwonly, Null :: kwdef, kwarg)
        }
    | COMMA vfpdef EQ test vfpkwonly_args {
        match $5 with (kwonly, kwdef, kwarg) ->
            ($2 :: kwonly, $4 :: kwdef, kwarg)
        }
    | COMMA vfpkwargs { ([], [], Some $2) }
    | { ([], [], None) }
;

vfpkwargs:
    POW vfpdef COMMA? { $2 }
;

stmt:
    | simple_stmt { $1 : stmt list } 
    | compound_stmt { List.cons $1 [] }
;

simple_stmt:
    | separated_nonempty_list(SEMICOL, small_stmt) NEWLINE      { $1 }
    | separated_nonempty_list(SEMICOL, small_stmt) SEMICOLEND   { $1 }
;

small_stmt:
    | expr_stmt    { $1 }
    | del_stmt     { $1 }
    | pass_stmt    { $1 }
    | flow_stmt    { $1 }
    | import_stmt  { $1 }
    | global_stmt  { $1 }
    | nonlocal_stmt { $1 }
    | assert_stmt  { $1 }
;    

expr_stmt:
    | testlist_star_expr    { Expr $1 }
    | testlist_star_expr annassign { AnnAssign ($1, fst $2, snd $2) }
    | testlist_star_expr augassign yield_expr  { AugAssign ($1, $2, $3) }
    | testlist_star_expr augassign testlist    { AugAssign ($1, $2, $3) }
    | testlist_star_expr EQ expr_stmt_rh_lst { Assign ($1:: (fst $3), snd $3) }
;

expr_stmt_rh_lst:
    | expr_stmt_rh { [], $1 }
    | expr_stmt_rh EQ expr_stmt_rh_lst { $1 :: (fst $3), snd $3 }
;

expr_stmt_rh:
    | yield_expr    { $1 }
    | testlist_star_expr { $1 }
;    

annassign:
    | COLON test    { $2, None }
    | COLON test EQ test { $2, Some $4 }
;

test_starexpr:
    | test      { $1 }
    | star_expr { $1 }
;

testlist_star_expr:
    | nonempty_list(terminated(test_starexpr, COMMA))    { Tuple ($1, Load) }
    | separated_nonempty_list(COMMA, test_starexpr)         { match $1 with 
        | [s] -> s
        | l -> Tuple (l, Load) }
;

augassign:
    | ADDEQ         { Add }
    | SUBEQ         { Sub }
    | MULEQ         { Mult }
    | DIVEQ         { Div }
    | TDIVEQ        { FloorDiv }
    | MODEQ         { Mod }
    | ATEQ          { MatMult }
    | BITANDEQ      { BitAnd }
    | BITOREQ       { BitOr }
    | BITXOREQ      { BitXor }
    | RSHIFTEQ      { RShift }
    | LSHIFTEQ      { LShift }
    | POWEQ         { Pow }
;

del_stmt:
    DEL exprlist    { Delete $2 }
;

pass_stmt:
    PASS        { Pass } 
;

flow_stmt:
    | break_stmt    { $1 }
    | continue_stmt { $1 }
    | return_stmt   { $1 }
    | raise_stmt    { $1 }
    | yield_stmt    { Expr $1 }
;

break_stmt:
    BREAK         { Break }
;

continue_stmt:
    CONTINUE      { Continue }
;

return_stmt:
    | RETURN       { Return (None : expr option) }
    | RETURN testlist { Return (Some $2) }
;

yield_stmt:
    | yield_expr    { $1 }
;

raise_stmt:
    | RAISE                 { Raise ((None : expr option), (None : expr option)) }
    | RAISE test            { Raise (Some $2, (None : expr option)) }
    | RAISE test FROM test  { Raise (Some $2, Some $4) }
;

import_stmt:
    | import_name   { $1 }
    | import_from   { $1 }
;

import_name:
    IMPORT dotted_as_names  { Import $2 }
;

import_from:
    | FROM importfrom_module IMPORT MUL    
         { ImportFrom (fst $2, ["*", None], snd $2) }
    | FROM importfrom_module IMPORT LPAR import_as_names RPAR 
        { ImportFrom (fst $2, $5, snd $2) }
    | FROM importfrom_module IMPORT import_as_names  
        { ImportFrom (fst $2, $4, snd $2) }
;

importfrom_module:
    | dotted_name   { Some $1, Some 0 }
    | dot_level dotted_name { Some $2, Some $1 }
    | DOT dot_level     { (None : identifier option), Some (1 + $2) }
;    
     
dot_level:
    | DOT dot_or_zero { 1 + $2 }
;

dot_or_zero:
    | { 0 }
    | DOT dot_or_zero { 1 + $2 }
;

import_as_name:
    | name          { $1, None }
    | name AS name  { $1, Some $3 }
;

dotted_as_name:
    | dotted_name           {  $1, (None : identifier option)  }
    | dotted_name AS name   {  $1, Some $3  }
;

import_as_names:
    | import_as_name COMMA?     { [$1] }
    | import_as_name COMMA import_as_names { $1 :: $3 }
;

dotted_as_names:
    | dotted_as_name        { [$1] }
    | dotted_as_name COMMA dotted_as_names { $1 :: $3 }
;

dotted_name:
    | name                  { $1 }
    | name DOT dotted_name  { $1 ^ "." ^ $3 }  
;

global_stmt:
    GLOBAL separated_nonempty_list(COMMA, name)     { Global $2 }
;
    
nonlocal_stmt:
    NONLOCAL separated_nonempty_list(COMMA, name)   { Nonlocal $2 }
;

assert_stmt:
    | ASSERT test            { Assert ($2, None) }
    | ASSERT test COMMA test { Assert ($2, Some $4) }
;    

compound_stmt:
    | if_stmt       { $1 }
    | while_stmt    { $1 }
    | for_stmt      { $1 }
    | try_stmt      { $1 }
    | with_stmt     { $1 }
    | funcdef       { $1 }
    | classdef      { $1 }
    | decorated     { $1 }
    | async_stmt    { $1 }
;

async_stmt:
    | async_funcdef        { $1 }
    | ASYNC with_stmt     { 
        let (it, b) = breakWith $2 in AsyncWith (it, b)
        }
    | ASYNC for_stmt      {
        let (t, it, b, e) = breakFor $2 in
            AsyncFor (t, it, b, e) }
;

if_stmt:
    | IF test COLON suite elif_else { If ($2, $4, $5) }
;

elif_else:
    | { [] }
    | ELIF test COLON suite elif_else { [ If ($2, $4, $5) ] }
    | ELSE COLON suite      { $3 }
;        
    
while_stmt:
    | WHILE test COLON suite            { While ($2, $4, [] ) }
    | WHILE test COLON suite ELSE COLON suite { While ($2, $4, $7) }
;    

for_stmt:
    | FOR exprlist IN testlist COLON suite  { match $2 with
        | [e]  -> For (e, $4, $6, [])
        | l -> For (Tuple(l, Store), $4, $6, []) }
    | FOR exprlist IN testlist COLON suite ELSE COLON suite    { match $2 with
        | [e] -> For (e, $4, $6, $9)
        | l -> For (Tuple(l, Store), $4, $6, $9) }
;

try_stmt:
    | TRY COLON suite except_handlerlist    { Try ($3, $4, [], []) }
    | TRY COLON suite except_handlerlist ELSE COLON suite  { Try ($3, $4, $7, []) }
    | TRY COLON suite except_handlerlist FINALLY COLON suite    { Try ($3, $4, [], $7) }
    | TRY COLON suite except_handlerlist ELSE COLON suite FINALLY COLON suite  { Try ($3, $4, $7, $10) }
    | TRY COLON suite FINALLY COLON suite { Try ($3, [], [], $6) }
;

except_handlerlist:
    nonempty_list(except_handler)   { $1 }
;

except_handler:
    except_clause COLON suite   { ExceptHandler (fst $1, snd $1, $3) }
;

except_clause:
    | EXCEPT                { (None : expr option), (None : identifier option) }
    | EXCEPT test           { Some $2, (None : identifier option) }
    | EXCEPT test AS name   { Some $2, Some $4 }     
;

with_stmt:
    WITH separated_nonempty_list(COMMA, with_item) COLON suite    { With ( $2, $4) }
;

with_item:
    | test          { ( $1, (None : expr option) ) }
    | test AS expr  { ( $1, Some $3) }
;

suite:
    | simple_stmt                   { $1 }
    | NEWLINE INDENT stmt+ DEDENT   { List.concat $3 }   
;

test:
    | or_test                       { $1 }         
    | or_test IF or_test ELSE test  { IfExp($3, $1, $5) }
    | lambdef                     { $1 }
;

test_nocond:
    | or_test           { $1 }
    | lambdef_nocond    { $1 }    
;    

lambdef:
    | LAMBDA varargslist COLON test { Lambda ($2, $4) }
;

lambdef_nocond:
    | LAMBDA varargslist COLON test_nocond { Lambda ($2, $4) }
;

or_test:
    | separated_nonempty_list(OR, and_test) { match $1 with
        | [s] -> s
        | l -> BoolOp(Or, l) }
;

and_test:
    | separated_nonempty_list(AND, not_test)    { match $1 with
        | [s] -> s
        | l -> BoolOp(And, l) }
;        

not_test:
    | NOT not_test      { UnaryOp(Not, $2) }
    | comparison        { $1 }
;

comparison:
    | expr              { $1 }
    | expr comp_list    { Compare ($1, fst $2, snd $2) }
;

comp_list:
    | comp_op expr comp_list_empty    { $1 :: (fst $3), $2 :: (snd $3) }
;            

comp_list_empty:
    | { [], [] }
    | comp_op expr comp_list_empty    { $1 :: (fst $3), $2 :: (snd $3) }
;

comp_op:
   | LT             { Lt }
   | GT             { Gt }
   | EQUAL          { Eq }
   | GE             { GtE }
   | LE             { LtE }
   | NEQ            { NotEq }
   | IN             { In }
   | NOT IN         { NotIn }
   | IS             { Is }
   | IS NOT         { IsNot }
;

star_expr:
    MUL expr       { Starred ($2, Load) }
;     

expr:
    | xor_expr      { $1 }
    | expr BITOR xor_expr { BinOp($1, BitOr, $3) }
;

xor_expr:
    | and_expr      { $1 }
    | xor_expr BITXOR and_expr  { BinOp($1, BitXor, $3) }
;

and_expr:
    | shift_expr                    { $1 }
    | and_expr BITAND shift_expr    { BinOp($1, BitAnd, $3) }
;

shift_expr:
    | arith_expr                    { $1 }
    | shift_expr LSHIFT arith_expr  { BinOp($1, LShift, $3) }
    | shift_expr RSHIFT arith_expr  { BinOp($1, RShift, $3) }
;

arith_expr:
    | term                         { $1 }
    | arith_expr ADD term          { BinOp($1, Add, $3) }    
    | arith_expr SUB term          { BinOp($1, Sub, $3) }    
;

term:
    | factor                { $1 }
    | term term_op factor   { BinOp ($1, $2, $3) }
;

term_op:
    | MUL   { Mult }      
    | AT    { MatMult }
    | DIV   { Div }
    | MOD   { Mod }
    | TDIV  { FloorDiv }
;    

factor:
    | power             { $1 }
    | factor_op factor  { UnaryOp ($1, $2) }
;

factor_op:
    | ADD       { UAdd }
    | SUB       { USub }
    | BITNOT    { Invert }
;
            
power:
    | atom_expr             { $1 }            
    | atom_expr POW factor  { BinOp($1, Pow, $3) }
;

atom_expr:
    | atom_trailer          { $1 }
    | AWAIT atom_trailer    { Await ($2) }
;

atom_trailer:
    | atom                      { $1 }
    | atom_trailer LPAR RPAR    { Call ($1, [], []) }
    | atom_trailer LPAR arglist RPAR { Call ($1, fst $3, snd $3) }
    | atom_trailer LPAR arglist COMMARPAR { Call ($1, fst $3, snd $3) }
    | atom_trailer LSQ subscriptlist RSQ { match $3 with
        | [s] -> Subscript ($1, s, Load)
        | l -> Subscript ($1, ExtSlice l, Load)
        }
    | atom_trailer LSQ subscriptlist COMMARSQ {Subscript ($1, ExtSlice $3, Load) }
    | atom_trailer DOT name     { Attribute ($1, $3, Load) }
;

atom:
    | atom_tuple        { $1 }
    | atom_list         { $1 }
    | atom_dict         { $1 }
    | name              { Name ($1, Load) }
    | number            { Num $1 }
    | strings           { Str $1 }
    | bytes             { Bytes $1 }
    | DOT DOT DOT       { Ellipsis }
    | NONE              { NameConstant SNone }
    | TRUE              { NameConstant True }
    | FALSE             { NameConstant False }
;     
 
/* Iterable cannot be used in comprehension : Star_expr forbidden */
atom_tuple:
    | LPAR RPAR                 { Tuple ([], Load) }
    | LPAR yield_expr RPAR      { $2 }                
    | LPAR separated_nonempty_list(COMMA, test_starexpr) RPAR   { match $2 with
        | [s] -> s
        | l -> Tuple(l, Load) }
    | LPAR separated_nonempty_list(COMMA, test_starexpr) COMMARPAR   { Tuple($2, Load) }
    | LPAR test comp_for RPAR   { GeneratorExp($2, $3) }
;

/* Iterable cannot be used in comprehension : Star_expr forbidden */
atom_list:
    | LSQ RSQ               { List ([], Load) }
    | LSQ separated_nonempty_list(COMMA, test_starexpr) RSQ { List ($2, Load) }
    | LSQ separated_nonempty_list(COMMA, test_starexpr) COMMARSQ { List ($2, Load) }
    | LSQ test comp_for RSQ { ListComp($2, $3) }
;

/* Iterable cannot be used in comprehension : Star_expr forbidden */
atom_dict:
    | LBRACE RBRACE                 { Dict ([], []) }
    | LBRACE test COLON test comp_for RBRACE  { DictComp ($2, $4, $5) }
    | LBRACE dict_elts  RBRACE      { Dict (fst $2, snd $2) }
    | LBRACE dict_elts  COMMARBRA   { Dict (fst $2, snd $2) }
    | LBRACE test comp_for RBRACE   { SetComp($2, $3) }
    | LBRACE separated_nonempty_list(COMMA, test_starexpr) RBRACE  { Set $2 } 
    | LBRACE separated_nonempty_list(COMMA, test_starexpr) COMMARBRA  { Set $2 } 
;

number:
    | INT   { Int $1 }
    | FLOAT { Float $1 }
    | IMAG  { Imag $1 }
;

strings:
    | STR           { $1 }
    | STR strings   { ($1) ^ ($2) }
;

bytes:
    | BYTES         { $1 }
    | BYTES bytes   { ($1) ^ ($2) }
;    

subscriptlist:
    separated_nonempty_list(COMMA, subscript) { $1 }
;

subscript:
    | test                      { Index $1 }
    | test? COLON test?     { Slice ($1,$3, None) }
    | test? COLON test? COLON test?     { Slice ($1,$3, $5) }
;

expr_star:
    | expr          { $1 }
    | star_expr     { $1 }
;

exprlist:
   | nonempty_list(terminated(expr_star, COMMA))  { $1 }
   | separated_nonempty_list(COMMA, expr_star)    { $1 }
;
    
testlist:
   | nonempty_list(terminated(test, COMMA)) { Tuple($1, Load) } 
   | separated_nonempty_list(COMMA, test)  { match $1 with
        | [s] -> s
        | l -> Tuple (l, Load) }
;

dict_elts:
    | test COLON test  { [$1], [$3] }
    | POW expr         { [Null], [$2] }
    | test COLON test COMMA dict_elts   { $1 :: (fst $5), $3 :: (snd $5)  }
    | POW expr COMMA dict_elts   { Null :: (fst $4), $2 :: (snd $4) }
;

classdef:
    | CLASS name COLON suite        { ClassDef ($2, [], [], $4, []) } 
    | CLASS name LPAR RPAR COLON suite { ClassDef ($2, [], [], $6, []) }
    | CLASS name LPAR arglist RPAR COLON suite { ClassDef ($2, fst $4, snd $4, $7, []) } 
    | CLASS name LPAR arglist COMMARPAR COLON suite { ClassDef ($2, fst $4, snd $4, $7, []) } 
;

arglist:
    separated_nonempty_list(COMMA, argument)  { 
        let rec result = function
            | [(a, b)] -> a, b
            | (a, b) :: l -> let (c, d) = result l in a @ c, b @ d
            | _ -> assert false 
        in result $1            
        }
;

argument:
    | test          { ([$1], [] ) } 
    | test comp_for { ([GeneratorExp($1, $2)], []) }
    | name EQ test  { ([], [(Some $1, $3)]) }
    | POW test      { ([], [((None : identifier option), $2)]) }
    | MUL test      { ([$2], []) }
;    

comp_for:
    | nonempty_list(comp_for1)    { $1 }  
;

comp_for1:
    | FOR exprlist IN or_test list(comp_if) { match $2 with
        | [s] -> (s, $4, $5, false)
        | l -> (Tuple(l, Store), $4, $5, false) }
    | ASYNC FOR exprlist IN or_test list(comp_if) { match $3 with  
        | [s] -> (s, $5, $6, true)
        | l -> (Tuple(l, Store), $5, $6, true) }
;          

comp_if:
    IF test_nocond  { $2 }
;

yield_expr:
    | YIELD             { Yield (None : expr option) }
    | YIELD FROM test   { YieldFrom $3 }
    | YIELD testlist    { Yield (Some $2) }

name:
    IDENT   { $1 }
;
