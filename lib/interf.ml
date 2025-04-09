(* Interface avec le parser, contient les fonctions principales utilisées par main.ml *)

(* Exception personnalisée levée si une erreur de parsing est rencontrée.
   Elle contient : l'erreur, la ligne, la colonne, le jeton fautif, et le reste du texte *)
   exception ParseLexError of exn * (int * int * string * string)

   (* Fonction qui exécute le parser à partir d’un buffer lexical.
      Si une erreur se produit, elle capture les infos utiles et lève une exception *)
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
   
   (* Affiche une erreur de parsing de façon lisible pour l'utilisateur *)
   let print_parse_error fn_option (line,cnum,tok,tail) =
     print_string (
       (match fn_option with
        | None -> ""
        | Some fn -> "Parsing error in file: " ^ fn) ^
       " on line: " ^ string_of_int line ^
       " column: " ^ string_of_int cnum ^
       " token: "  ^ tok ^
       "\nrest: "  ^ tail ^ "\n"
     )
   
   (* Lance le parser depuis un fichier ou depuis l’entrée standard (terminal).
      En cas d’erreur, elle est affichée proprement. *)
   let run_parser_error_reporting fn_option = 
     let chan = match fn_option with 
       | None -> Stdlib.stdin
       | Some fn -> open_in fn
     in
     let lexbuf = Lexing.from_channel chan in 
     try run_parser lexbuf
     with ParseLexError (_e, errinfo) -> 
       print_parse_error fn_option errinfo;
       failwith "Stopped execution."
   
   (* Mode interactif : permet d’écrire une requête à la main dans le terminal *)
   let run_interactive () = 
     while true do
       Printf.printf ">> %!";
       let e = run_parser_error_reporting None in
       Printf.printf "%s\n" (Lang.show_prog e)
     done
   
   (*  Partie ajoutée : fonction pour lancer tous les tests de typage  *)
   let run_tests () =
     Typing.test_1 (); Typing.test_2 (); Typing.test_3 ();
     Typing.test_4 (); Typing.test_5 (); Typing.test_6 ();
     Typing.test_7 (); Typing.test_8 (); Typing.test_9 ();
     Typing.test_10 (); Typing.test_11 (); Typing.test_12 (); Typing.test_13 ();
     Typing.test_norm ();
     Typing.test_norm_invalid ()
   
   (* Fonction principale pour lancer l’analyse d’un fichier :
      - Parsing du fichier
      - Normalisation
      - Vérification de types
      - Affichage du programme parsé
      - Lancement des tests *)
  let run_file fn = 
    let p = run_parser_error_reporting (Some fn) in
    let np = Typing.typecheck true (Instr.normalize_prog p) in
    (*let tc_rep = Typing.typecheck true np in*)
    let State(g, tab, _mn) = Sem.exec np in
    Printf.printf "%s\n" (Sem.show_db_graph_struct g);
    Printf.printf "%s\n" (Sem.show_vname_nodeid_table tab);
    Display.output_table tab;
    Display.output_graph_struct g;

    Printf.printf "---------------- TESTS --------------\n";
    
    run_tests ()
   
   (* Affiche un message d’aide sur comment exécuter le programme *)
   let print_help () =
     print_string "Run as:\n Code_Graph [h | i | f <filename> ] \n "
   
