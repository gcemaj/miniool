{
  open Parser;;
  exception SyntaxError;;
}

let blank = [' ' '\r' '\t']
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let iden = alpha (alpha | digit | '_')*

rule miniool = parse
    blank { miniool lexbuf } (* skip blanks and tabs *)
  | "debug"         {DEBUG}
  | "var"           {VAR}
  | "proc"          {PROC}
  | "if"            {IF}
  | "then"          {THEN}
  | "else"          {ELSE}
  | "while"         {WHILE}
  | "do"            {DO}
  | "malloc"        {MALLOC}
  | "true"          {BOOL (true)}
  | "false"         {BOOL (false)}
  | "."             {DOT}
  | "null"          {NULL}
  | "skip"          {SKIP}
  | "and"           {AND}
  | "or"            {OR}
  | "not"           {NOT}
  | "|||"           {PARALLEL}
  | "atom"          {ATOM}
  | ['\n' ]         {EOL}
  | iden as idt     {IDENT idt}
  | digit+ as num   {NUM (int_of_string num)}
  | ','             {COMA}
  | ':'             {COLON}
  | ';'             {SEMICOLON}
  | '='             {EQUALS}
  | '>'             {GREATERTHAN}
  | '<'             {LESSTHAN}
  | '('             {LPAREN}
  | ')' 	          {RPAREN}
  | '{'             {LCURL}
  | '}' 	          {RCURL}
  | '+'             {PLUS}
  | '-'             {MINUS}
  | '*'             {TIMES}
  | '/'             {DIV}
  | '%'             {MOD}
  | _               {raise SyntaxError}
  | eof             {raise SyntaxError}