open Format
open Lexing

let filename = ref ""

let usage = "usage: ocaml-python3 [options] file.py"

let set_var v s = v:=s

let options = []

let error_loc pos = 
    let line = pos.pos_lnum in
    let col = pos.pos_cnum-pos.pos_bol + 1 in
    eprintf "File \"%s\", line %d, characters %d-%d:\n" !filename line (col-1) col

let localisation2 (pos1, pos2) = 
    let l = pos1.pos_lnum in
    let c1 = pos1.pos_cnum - pos1.pos_bol + 1 in
    let c2 = pos2.pos_cnum - pos2.pos_bol + 1 in
    eprintf "File \"%s\", line %d, characters %d-%d:\n" !filename l c1 c2

let print_in_file ~file p = 
    let c = open_out file in
    let fmt = formatter_of_out_channel c in
    fprintf fmt "%s" p;
    pp_print_flush fmt ();
    close_out c

let () = 
    Arg.parse options (set_var filename) usage;
    
    if !filename = "" then
        (eprintf "No input file\n@?";
         exit 2);

    if not (Filename.check_suffix !filename ".py") then (
        eprintf "Files must have the .py extension\n@?";
        Arg.usage options usage;
        exit 2);

    let f = open_in !filename in
    let buf = Lexing.from_channel f in

    try
        let _p = Parser.file_input Lexer.next_token buf in
        close_in f;

    with
        |Lexer.LexingError s ->
        error_loc (Lexing.lexeme_start_p buf);
        eprintf "%s\n@?" s;
        exit 1;
        |Parser.Error ->
        error_loc (Lexing.lexeme_start_p buf);
        eprintf "Syntax error\n@?";
        exit 1; 

