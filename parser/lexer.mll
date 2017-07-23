(* Lexer for Python 3 *)
(* The description of the lexical analysis can be found at
   https://docs.python.org/3/reference/lexical_analysis.html *)

{
    open Lexing
    open Parser 

    exception LexingError of string

    let keywords = [
        "False", FALSE;
        "None", NONE;
        "True", TRUE;
        "and", AND;
        "as", AS;
        "assert", ASSERT;
        "break", BREAK;
        "class", CLASS;
        "continue", CONTINUE;
        "def", DEF;
        "del", DEL;
        "elif", ELIF;
        "else", ELSE;
        "except", EXCEPT;
        "finally", FINALLY;
        "for", FOR;
        "from", FROM;
        "global", GLOBAL;
        "if", IF;
        "import", IMPORT;
        "in", IN;
        "is", IS;
        "lambda", LAMBDA;
        "nonlocal", NONLOCAL;
        "not", NOT;
        "or", OR;
        "pass", PASS;
        "raise", RAISE;
        "return", RETURN;
        "try", TRY;
        "while", WHILE;
        "with", WITH;
        "yield", YIELD;
        (* We consider that the next two are keywords *)
        "async", ASYNC;
        "await", AWAIT;
    ]

    let id_or_kwd s = try List.assoc s keywords with _ -> IDENT s

    let newline lexbuf =
        let pos = lexbuf.lex_curr_p in
        lexbuf.lex_curr_p <- {
            pos with pos_lnum = pos.pos_lnum + 1;
            pos_bol = pos.pos_cnum
        }

    let buffer = ref (Buffer.create 0)

    let stack = ref [0]
    let rec unindent n = match !stack with
        | m :: _ when m = n -> []
        | m :: st when m > n -> stack := st; DEDENT :: unindent n
        | _ -> raise (LexingError "incorrect indentation")

}

let space = ' ' | '\t' 
let endline = '\n' | '\r' | "\r\n"
let comment = "#" [^ '\n' '\r']*

(* Identifiers *)

let id_continue = ['0' - '9' 'a' - 'z' 'A' - 'Z' '_'] (* TODO : Add unicode from OOO080 to 10FFFF *)
let id_start = ['a' - 'z' 'A' - 'Z' '_'] (* TODO : Add unicode from OOO080 to 10FFFF *)
let identifier = id_start id_continue*

(* Strings *)
let stringprefix =  "u" | "U" 
let rawstringprefix = "r" | "R" 
let escapeseq = "\\" _

(* Bytestrings *)
let byteprefix = "b" | "B" 
let rawbyteprefix = "br" | "Br" | "bR" | "BR" | "rb" | "rB" | "Rb" | "RB"

(* Integer literals *)

let digit = ['0' - '9']
let bindigit = ['0' '1']
let octdigit = ['0' - '7']
let hexdigit = digit | (['a' - 'f' 'A' - 'F'])
let nonzerodigit = ['1' - '9']
let decinteger = nonzerodigit (['_'] | digit)* | '0' + (['_'] '0')*
let bininteger = '0' ('b' | 'B') (['_'] bindigit)+
let octinteger = '0' ('o' | 'O') (['_'] octdigit)+
let hexinteger = '0' ('x' | 'X') (['_'] hexdigit)+
let integer = decinteger | bininteger | octinteger | hexinteger

(* Floating point literals *)

let digitpart = digit ('_'? digit)*
let exponent = ('e' | 'E') ['+' '-'] digitpart 
let fraction = '.' digitpart
let pointfloat = digitpart* fraction | digitpart "."
let exponentfloat = (digitpart | pointfloat) exponent
let floatnumber = pointfloat | exponentfloat

(* Imaginary literals *)
let imagnumber = (floatnumber | digitpart) ("j" | "J")

rule token = parse
    | (space | comment)+        { token lexbuf }
    (* Line-joining *)
    | '\\' endline              { newline lexbuf; token lexbuf }
    | ';' (space | comment)* '\n' { newline lexbuf;
                                    let n = indentation lexbuf in
                                    match !stack with
                                        | m :: _ when m < n ->
                                            stack := n :: !stack;
                                            [SEMICOLEND;  INDENT]
                                        | _ -> SEMICOLEND  :: unindent n
                                }
    | '\n'                      { newline lexbuf;
                                    let n = indentation lexbuf in
                                    match !stack with
                                        | m :: _ when m < n ->
                                            stack := n :: !stack;
                                            [NEWLINE; INDENT]
                                        | _ -> NEWLINE :: unindent n
                                }
    | identifier as id          { [id_or_kwd id] } (* TODO : Check if the identifier has a valid unicode name *)
    (* Operators *)
    | '+'                       { [ADD] }
    | '-'                       { [SUB] }
    | '*'                       { [MUL] }
    | "**"                      { [POW] }
    | '/'                       { [DIV] }
    | "//"                      { [TDIV] }
    | "%"                       { [MOD] }
    | '@'                       { [AT] }
    | "<<"                      { [LSHIFT] }
    | ">>"                      { [RSHIFT] }
    | "&"                       { [BITAND] }
    | "|"                       { [BITOR] }
    | "^"                       { [BITXOR] }
    | "~"                       { [BITNOT] }
    | "<"                       { [LT] }
    | ">"                       { [GT] }
    | "<="                      { [LE] }
    | ">="                      { [GE] }
    | "=="                      { [EQUAL] }
    | "<>"                      { [NEQ] }
    | "!="                      { [NEQ] }
    (* Delimiters *)
    | ',' space* ']'            { [COMMARSQ] }
    | ',' space* ')'            { [COMMARPAR] }
    | ',' space* '}'            { [COMMARBRA] }
    | "("                       { [LPAR] }
    | ")"                       { [RPAR] }
    | "["                       { [LSQ] }
    | "]"                       { [RSQ] }
    | "{"                       { [LBRACE] }
    | "}"                       { [RBRACE] }
    | ","                       { [COMMA] }
    | ":"                       { [COLON] }
    | "."                       { [DOT] }
    | ";"                       { [SEMICOL] }
    | "="                       { [EQ] }
    | "->"                      { [ARROW] }
    | "+="                      { [ADDEQ] }
    | "-="                      { [SUBEQ] }
    | "*="                      { [MULEQ] }
    | "/="                      { [DIVEQ] }
    | "//="                     { [TDIVEQ] }
    | "%="                      { [MODEQ] }
    | "@="                      { [ATEQ] }
    | "&="                      { [BITANDEQ] }
    | "|="                      { [BITOREQ] }
    | "^="                      { [BITXOREQ] }
    | ">>="                     { [RSHIFTEQ] }
    | "<<="                     { [LSHIFTEQ] }
    | "**="                     { [POWEQ] }
    (* Numbers *)
    | integer as i              { [INT (int_of_string i)] }
    | floatnumber as f          { [FLOAT (float_of_string f)] }
    | imagnumber  as i          { [IMAG i] }
    (* Long string literals *)
    | rawbyteprefix "\"\"\"" 
        { [BYTES (let x = unesc_long_dq_prefix lexbuf in String.concat "" (List.map (String.make 1) x))] }
    | rawstringprefix  "\"\"\"" 
        { [STR (let x = unesc_long_dq_prefix lexbuf in String.concat "" (List.map (String.make 1) x))] } 
    | byteprefix "\"\"\""
        { [BYTES (let x = long_dq_prefix lexbuf in String.concat "" (List.map (String.make 1) x))] }
    | stringprefix? "\"\"\"" 
        { [STR (let x = long_dq_prefix lexbuf in String.concat "" (List.map (String.make 1) x))] }
    | rawbyteprefix  "'''"
        { [BYTES (let x = unesc_long_sq_prefix lexbuf in String.concat "" (List.map (String.make 1) x))] } 
    | rawstringprefix  "'''" 
        { [STR (let x = unesc_long_sq_prefix lexbuf in String.concat "" (List.map (String.make 1) x))] } 
    | byteprefix  "'''"
        { [BYTES (let x = long_sq_prefix lexbuf in String.concat "" (List.map (String.make 1) x))] }
    | stringprefix? "'''" 
        { [STR (let x = long_sq_prefix lexbuf in String.concat "" (List.map (String.make 1) x))] }
    (* Short string literals *)
    | rawbyteprefix  '\''  
        { [BYTES (let x = unesc_sq_prefix lexbuf in String.concat "" (List.map (String.make 1) x))] }
    | rawstringprefix  '\''
        { [STR (let x = unesc_sq_prefix lexbuf in String.concat "" (List.map (String.make 1) x))] } 
    | byteprefix '\'' 
        { [BYTES (let x = sq_prefix lexbuf in String.concat "" (List.map (String.make 1) x))] }
    | stringprefix? '\''
        { [STR  (let x = sq_prefix lexbuf in String.concat "" (List.map (String.make 1) x))] }
    | rawbyteprefix  '"'
        { [BYTES (let x = unesc_dq_prefix lexbuf in String.concat "" (List.map (String.make 1) x))] } 
    | rawstringprefix  '"'
        { [STR (let x = unesc_dq_prefix lexbuf in String.concat "" (List.map (String.make 1) x))] } 
    | byteprefix '"'
        { [BYTES (let x = dq_prefix lexbuf in String.concat "" (List.map (String.make 1) x))] }
    | stringprefix? '"' 
        { [STR (let x = dq_prefix lexbuf in String.concat "" (List.map (String.make 1) x))] }
    | eof                       { [EOF] }
    | _ as c                    { raise (LexingError ("illegal character (unicode not supported) : " ^ String.make 1 c)) }
    
and indentation = parse
    | (space | comment)* '\n'
        { newline lexbuf; indentation lexbuf }
    | space* as s   { String.length s }

and unesc_dq_prefix = parse
    | eof           { raise (LexingError ("unterminated string")) } 
    | "\\\n"        { newline lexbuf; unesc_dq_prefix lexbuf }
    | "\""          { [] }
    | _ as c        { (c) :: (unesc_dq_prefix lexbuf) }    

and dq_prefix = parse
    | eof           { raise (LexingError ("unterminated string")) } 
    | "\\\n"        { newline lexbuf; dq_prefix lexbuf }
    | "\""          { [] }
    | "\\\\"        {  '\\' :: (dq_prefix lexbuf) }
    | "\\\'"        {  '\'' :: (dq_prefix lexbuf) }
    | "\\\""        {  '"' :: (dq_prefix lexbuf)  }
    | "\\a"         { (Char.chr 7) :: (dq_prefix lexbuf) }
    | "\\b"         { ('\b') :: (dq_prefix lexbuf) }
    | "\\f"         { (Char.chr 12) :: (dq_prefix lexbuf) } 
    | "\\n"         { ('\n') :: (dq_prefix lexbuf) }
    | "\\r"         { ('\r'):: (dq_prefix lexbuf) }
    | "\\t"         { ('\t')::(dq_prefix lexbuf) }
    | "\\v"         { (Char.chr 11) :: (dq_prefix lexbuf) }
    | _ as c        { (c) :: (dq_prefix lexbuf) }    

and unesc_sq_prefix = parse
    | eof           { raise (LexingError ("unterminated string")) } 
    | "\\\n"        { newline lexbuf; unesc_sq_prefix lexbuf }
    | "\'"          { [] }
    | _ as c        { (c) :: (unesc_sq_prefix lexbuf) }    

and sq_prefix = parse
    | eof           { raise (LexingError ("unterminated string")) } 
    | "\\\n"        { newline lexbuf; sq_prefix lexbuf }
    | "\'"          { [] }
    | "\\\\"        {  '\\' :: (sq_prefix lexbuf) }
    | "\\\'"        {  '\'' :: (sq_prefix lexbuf) }
    | "\\\""        {  '"' :: (sq_prefix lexbuf)  }
    | "\\a"         { (Char.chr 7) :: (sq_prefix lexbuf) }
    | "\\b"         { ('\b') :: (sq_prefix lexbuf) }
    | "\\f"         { (Char.chr 12) :: (sq_prefix lexbuf) } 
    | "\\n"         { ('\n') :: (sq_prefix lexbuf) }
    | "\\r"         { ('\r'):: (sq_prefix lexbuf) }
    | "\\t"         { ('\t')::(sq_prefix lexbuf) }
    | "\\v"         { (Char.chr 11) :: (sq_prefix lexbuf) }
    | _ as c        { (c) :: (sq_prefix lexbuf) }    

and unesc_long_sq_prefix = parse
    | eof           { raise (LexingError ("unterminated string")) } 
    | "\\\n"        { newline lexbuf; unesc_long_sq_prefix lexbuf }
    | "\'\'\'"      { [] }
    | _ as c        { (c) :: (unesc_long_sq_prefix lexbuf) }    

and long_sq_prefix = parse
    | eof           { raise (LexingError ("unterminated string")) } 
    | "\\\n"        { newline lexbuf; long_sq_prefix lexbuf }
    | "\'\'\'"      { [] }
    | "\\\\"        {  '\\' :: (long_sq_prefix lexbuf) }
    | "\\\'"        {  '\'' :: (long_sq_prefix lexbuf) }
    | "\\\""        {  '"' :: (long_sq_prefix lexbuf)  }
    | "\\a"         { (Char.chr 7) :: (long_sq_prefix lexbuf) }
    | "\\b"         { ('\b') :: (long_sq_prefix lexbuf) }
    | "\\f"         { (Char.chr 12) :: (long_sq_prefix lexbuf) } 
    | "\\n"         { ('\n') :: (long_sq_prefix lexbuf) }
    | "\\r"         { ('\r'):: (long_sq_prefix lexbuf) }
    | "\\t"         { ('\t')::(long_sq_prefix lexbuf) }
    | "\\v"         { (Char.chr 11) :: (long_sq_prefix lexbuf) }
    | _ as c        { (c) :: (long_sq_prefix lexbuf) }    

and unesc_long_dq_prefix = parse
    | eof           { raise (LexingError ("unterminated string")) } 
    | "\\\n"        { newline lexbuf; unesc_long_dq_prefix lexbuf }
    | "\"\"\""      { [] }
    | _ as c        { (c) :: (unesc_long_dq_prefix lexbuf) }    

and long_dq_prefix = parse
    | eof           { raise (LexingError ("unterminated string")) } 
    | "\\\n"        { newline lexbuf; long_dq_prefix lexbuf }
    | "\"\"\""      { [] }
    | "\\\\"        {  '\\' :: (long_dq_prefix lexbuf) }
    | "\\\'"        {  '\'' :: (long_dq_prefix lexbuf) }
    | "\\\""        {  '"' :: (long_dq_prefix lexbuf)  }
    | "\\a"         { (Char.chr 7) :: (long_dq_prefix lexbuf) }
    | "\\b"         { ('\b') :: (long_dq_prefix lexbuf) }
    | "\\f"         { (Char.chr 12) :: (long_dq_prefix lexbuf) } 
    | "\\n"         { ('\n') :: (long_dq_prefix lexbuf) }
    | "\\r"         { ('\r'):: (long_dq_prefix lexbuf) }
    | "\\t"         { ('\t')::(long_dq_prefix lexbuf) }
    | "\\v"         { (Char.chr 11) :: (long_dq_prefix lexbuf) }
    | _ as c        { (c) :: (long_dq_prefix lexbuf) }    
(* TODO : Deal with \ooo, \xhh, \uxxxx and \Uxxxx according to the doc *)
 
 {
    (* Useful for debug *)
    let print_token = function

        | IDENT s -> "IDENT " ^ s ^" "
        | INT i -> "INT " ^ (string_of_int i) ^ " "
        | FLOAT f-> "FLOAT "^ (string_of_float f) ^ " "
        | IMAG s-> "IMAG " ^ s ^ " "
        | STR s-> "STR " ^ s ^ " "
        | BYTES s -> "BYTES " ^ s ^" "
        
        | INDENT  -> "INDENT "
        | DEDENT -> "DEDENT "
        | NEWLINE -> "NEWLINE "
        | EOF -> "EOF "
        
        (* Operators *)
        | ADD -> "ADD "
        | SUB -> "SUB "
        | MUL -> "MUL "
        | POW -> "POW "
        | DIV -> "DIV "
        | TDIV -> "TDIV "
        | MOD -> "MOD "
        | AT -> "AT "
        
        | LSHIFT -> "LSHIFT "
        | RSHIFT -> "RSHIFT "
        | BITAND -> "BITAND "
        | BITOR -> "BITOR "
        | BITXOR -> "BITXOR "
        | BITNOT -> "BITNOT "
        
        | LT -> "LT "
        | GT -> "GT "
        | LE -> "LE "
        | GE -> "GE "
        | EQUAL -> "EQUAL "
        | NEQ -> "NEQ "
        
        | SEMICOLEND -> "SEMICOLEND "(* ; at the end of a line *)
        | COMMARSQ  -> "COMMARSQ " (* , ] *)
        | COMMARPAR -> "COMMAPAR " (* , ) *)
        | COMMARBRA -> "COMMARBRA " (* , } *)
        | LPAR -> "LPAR "
        |  RPAR -> "RPAR "
        | LSQ -> "LSQ "
        | RSQ -> "RSQ "
        | LBRACE -> "LBRACE "
        | RBRACE -> "RBRACE "
        | COMMA -> "COMMA "
        | COLON -> "COLON "
        | DOT -> "DOT "
        | SEMICOL -> "SEMICOL "
        | EQ -> "EQ "
        | ARROW -> "ARROW "
        
        | ADDEQ -> "ADDEQ "
        | SUBEQ -> "SUBEQ "
        | MULEQ -> "MULEQ "
        | DIVEQ -> "DIVEQ "
        | TDIVEQ -> "TDIVEQ "
        | MODEQ -> "MODEQ "
        | ATEQ -> "ATEQ "
        | BITANDEQ -> "BITANDEQ "
        | BITOREQ -> "BITOREQ "
        | BITXOREQ -> "BITXOREQ "
        | RSHIFTEQ -> "RSHIFTEQ "
        | LSHIFTEQ  -> "LSHIFTEQ "
        | POWEQ -> "POWEQ "

        | FALSE -> "FALSE "
        | NONE -> "NONE "
        | TRUE -> "TRUE "
        | AND -> "AND "
        | AS -> "AS "
        | ASSERT -> "ASSERT "
        | BREAK -> "BREAK "
        | CLASS -> "CLASS "
        | CONTINUE -> "CONTINUE "
        | DEF -> "DEF "
        | DEL -> "DEL "
        | ELIF -> "ELIF "
        | ELSE -> "ELSE "
        | EXCEPT -> "EXCEPT "
        | FINALLY -> "FINALLY "
        | FOR -> "FOR "
        | FROM -> "FROM "
        | GLOBAL -> "GLOBAl "
        | IF -> "IF "
        | IMPORT -> "IMPORT "
        | IN -> "IN "
        | IS -> "IS "
        | LAMBDA -> "LAMBDA "
        | NONLOCAL -> "NONLOCAL "
        | NOT -> "NOT "
        | OR -> "OR "
        | PASS -> "PASS "
        | RAISE -> "RAISE "
        | RETURN -> "RETURN "
        | TRY -> "TRY "
        | WHILE -> "WHILE "
        | WITH -> "WITH "
        | YIELD -> "YIELD "
        | AWAIT -> "AWAIT "
        | ASYNC -> "ASYNC "

      let next_token =
        let tokens = Queue.create () in
        fun lb -> 
            if Queue.is_empty tokens then begin
        let l = token lb in
        List.iter (fun t -> Queue.add t tokens) l
            end;
            Queue.pop tokens


}            
