open Parsing;;
(* open Lexer;; *)
try
  let lexbuf = Lexing.from_channel stdin in
  while true do
    try
      Parser.prog Lexer.miniool lexbuf
    with Parse_error ->
      (print_string "Syntax error ..." ; print_newline ()) ;
      clear_parser ()
    done
with Lexer.SyntaxError ->
  (print_string ":(" ; print_newline ()) ;
 ()
;;