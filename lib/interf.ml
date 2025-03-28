(* Interface functionality with the parser, contains the essential 
   functions for bin/main.ml
*)


exception ParseLexError of exn * (int * int * string * string)

(* Run parser by reading from lex buffer, 
   raise ParseLexError in case of error
 *)
let run_parser lexbuf = 
  try
    Parser.main Lexer.token lexbuf
  with exn ->
    begin
      let curr = lexbuf.Lexing.lex_curr_p in
      let line = curr.Lexing.pos_lnum in
      let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
      let tok = Lexing.lexeme lexbuf in
      let tail =  Lexer.ruleTail "" lexbuf in 
      raise (ParseLexError (exn,(line,cnum,tok,tail)))
    end


let print_parse_error fn_option (line,cnum,tok,tail) =
  print_string ((match fn_option with None -> "" | Some fn -> "Parsing error in file: " ^ fn) ^ 
                      " on line: " ^ (string_of_int line) ^ 
                      " column: " ^ (string_of_int cnum) ^
                      " token: "  ^ tok ^
                      "\nrest: "  ^ tail ^ "\n")
;;


(* Run parser either reading from standard in (for fn_option = None)
   or reading from a file name (for fn_option = Some fn)
   and report errors in a readable format
 *)
let run_parser_error_reporting fn_option = 
  let chan = (match fn_option with 
               None -> (Stdlib.stdin)
              | Some fn -> open_in fn) in
  let lexbuf = Lexing.from_channel chan in 
  try run_parser lexbuf
  with ParseLexError (_e, errinfo) -> 
    print_parse_error fn_option errinfo;
    failwith "Stopped execution."
;;

(* Run parser interactively in a read - print loop *)
let run_interactive () = 
  while true do
    Printf.printf ">> %!";
    let e = run_parser_error_reporting None in
    Printf.printf "%s\n" (Lang.show_prog e)
  done

(* Run the file with name fn (a string),
   including parsing, type checking, displaying the output
 *)
 (*
let run_file fn = 
  let p = run_parser_error_reporting (Some fn) in
  let np = Typing.typecheck true (Instr.normalize_prog p) in 
  let State(g, tab, _mn) = Sem.exec np in 
  Printf.printf "%s\n" (Sem.show_db_graph_struct g);
  Printf.printf "%s\n" (Sem.show_vname_nodeid_table tab);
  Display.output_table tab;
  Display.output_graph_struct g

*)

let run_tests () =
  Typing.test_1 (); Typing.test_2 (); Typing.test_3 ();
  Typing.test_4 (); Typing.test_5 (); Typing.test_6 ();
  Typing.test_7 (); Typing.test_8 (); Typing.test_9 ();
  Typing.test_10 (); Typing.test_11 (); Typing.test_12 (); Typing.test_13 ();
  Typing.test_norm ();
  Typing.test_norm_invalid ()
;;

let run_file fn = 
  let p = run_parser_error_reporting (Some fn) in
  let np = Instr.normalize_prog p in
  let _ = Typing.typecheck true np in
  Printf.printf "\n Parsing + Typage réussi !\n";
  Printf.printf "%s\n" (Lang.show_prog p);
  Printf.printf "---------------- TESTS --------------\n";
  run_tests ()
;;

  

(* Print the help message *)
let print_help () = print_string "Run as:\n Code_Graph [h | i | f <filename> ] \n "

