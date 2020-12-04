open Parsing;;
open Printf;;

let src_path = ref "";;
let err_str = "\027[31;1mError\027[0m: ";;

Arg.parse
  [("--verbose",
    Arg.Unit (fun () -> Flags.verbose := true),
    "Verbosly print steps of interpretation");
  ("--ast",
    Arg.Unit (fun () -> Flags.ast := true),
    "Print the AST instead of evaluating");
  ("--src", 
    Arg.String (fun file_path-> src_path:= file_path; Flags.interpreted := false),
    "File path to .mini file")]
  (fun _ -> ())
  "\nUsage: MiniOOL [--verbose] [-help|--help]";;

let run_buf buf =
  try
    Parser.prog Lexer.miniool buf;
  with 
    | Parse_error | Lexer.SyntaxError ->  printf "Syntax error ...\n%!";clear_parser ()
    | Evaluator.EvaluationError err -> fprintf stderr "%s%s\n%!" err_str err;clear_parser ()
    | Failure msg -> fprintf stderr "%s%s\n%!" err_str msg
    | Lexer.EoF -> ()
  ;;

if !Flags.interpreted then
  let lexbuf = Lexing.from_channel stdin in
  while true do
    run_buf lexbuf
  done
else
  let chan = open_in !src_path in
  let lines = ref (input_line chan) in
  try
    while true; do
      lines := sprintf "%s %s" !lines (input_line chan);
    done;
  with 
  | Lexer.EoF |End_of_file -> run_buf (Lexing.from_string (sprintf "%s\n" !lines)); 
  