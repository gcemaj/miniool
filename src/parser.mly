%{
  open Ast;;
  open AstPrinter;;
  open Evaluator;;
%}

/*LEXER TOKENS*/
%token DEBUG
%token VAR
%token PROC
%token IF THEN ELSE
%token WHILE DO
%token PARALLEL ATOM
%token MALLOC NULL SKIP
%token <bool> BOOL
%token <string> IDENT
%token <int> NUM
%token DOT SEMICOLON COLON COMA
%token AND OR NOT
%token EQUALS GREATERTHAN LESSTHAN
%token LPAREN RPAREN LCURL RCURL
%token PLUS MINUS TIMES DIV MOD
%token EOL
/*LEXER TOKENS*/

/*GRAMMAR TOKENS*/
%start prog /* the entry point */
%type <unit> prog
/*GRAMMAR TOKENS*/

%right ASSIGN
%left OR
%left AND
%nonassoc NOT
%left LESSTHAN GREATERTHAN EQUALS
%left PLUS MINUS
%left TIMES DIV
%left MOD
%nonassoc UMINUS
%left DOT


%% /* rules */
prog :
    ast = cmds EOL { if !Flags.ast then print_ast ast else eval_ast ast}
cmds :
| c = cmd SEMICOLON cs = cmds                                                                   { c :: cs }
| c = cmd                                                                                       { [c] }
|                                                                                               { [] }
cmd :
| VAR name = IDENT                                                                              { Declaration (ref name) }
| VAR name = IDENT EQUALS exp = expr                                                            { DeclarationAndAssigment ((ref name), exp) }
| name = IDENT EQUALS exp = expr                                                                { Assigment ((ref name), exp) }
| exp1 = expr DOT field = IDENT EQUALS exp2 = expr                                              { FieldAssignment (exp1, (ref field), exp2) }
| IF bool_exp= bool_expr THEN c1 = cmd ELSE c2 = cmd                                            { IfThenElseControlFlow (bool_exp, c1, c2) }
| IF bool_exp= bool_expr THEN c1 = cmd                                                          { IfThenControlFlow (bool_exp, c1) }
| WHILE bool_exp= bool_expr DO c1 = cmd                                                         { WhileControlFlow (bool_exp, c1) }
| c1 = cmd PARALLEL c2 = cmd                                                                    { ParallelFlow (c1, c2) }
| ATOM LPAREN c = cmd RPAREN                                                                    { AtomFlow (c) }
| exp = expr                                                                                    { Expression (exp) }
| LCURL c = cmds RCURL                                                                          { CmdSequence(c) }
| MALLOC LPAREN name = IDENT RPAREN                                                             { Malloc(ref name) }
| DEBUG LPAREN exp = expr RPAREN                                                                { Debug (exp) }
| SKIP                                                                                          { Skip }
bool_expr:
| b = BOOL                                                                                      { Bool (b) }
| exp = expr                                                                                    { BoolExpr (exp) }
| exp1 = expr GREATERTHAN exp2 = expr                                                           { BinaryIntegerComparatorExpr ( ( > ), exp1, exp2 ) }
| exp1 = expr GREATERTHAN EQUALS exp2 = expr                                                    { BinaryIntegerComparatorExpr ( ( >= ), exp1, exp2 ) }
| exp1 = expr LESSTHAN exp2 = expr                                                              { BinaryIntegerComparatorExpr ( ( < ), exp1, exp2 ) }
| exp1 = expr LESSTHAN EQUALS exp2 = expr                                                       { BinaryIntegerComparatorExpr ( ( <= ), exp1, exp2 ) }
| exp1 = expr EQUALS EQUALS exp2 = expr                                                         { BinaryIntegerComparatorExpr ( ( == ), exp1, exp2 ) }
| exp1 = bool_expr AND exp2 = bool_expr                                                         { BinaryBoolComparatorExpr ( ( && ), exp1, exp2 ) }
| exp1 = bool_expr OR exp2 = bool_expr                                                          { BinaryBoolComparatorExpr ( ( || ), exp1, exp2 ) }
| NOT exp = bool_expr                                                                           { UnaryBoolComparatorExpr ( ( not ), exp ) }
expr:
| number = NUM                                                                                  { Integer ( number ) }
| name = IDENT                                                                                  { Variable ( ref name ) }
| e1 = expr DOT field = IDENT                                                                   { Field (e1, ref field) }
| NULL                                                                                          { Null }
| PROC LPAREN arg_in = IDENT COMA arg_out = IDENT RPAREN COLON c = cmd                          { Procedure ( ref arg_in, ref arg_out, c ) }
| exp1 = expr LPAREN exp2 = expr RPAREN                                                         { Call (exp1, exp2) }
| e1 = expr PLUS e2 = expr                                                                      { BinaryArithmeticOperator ( ( + ), e1, e2 ) }
| e1 = expr MINUS e2 = expr                                                                     { BinaryArithmeticOperator ( ( - ), e1, e2 ) }
| e1 = expr DIV e2 = expr                                                                       { BinaryArithmeticOperator ( ( / ), e1, e2 ) }
| e1 = expr TIMES e2 = expr                                                                     { BinaryArithmeticOperator ( ( * ), e1, e2 ) }
| e1 = expr MOD e2 = expr                                                                       { BinaryArithmeticOperator ( ( mod ), e1, e2 ) }
| MINUS e = expr                                                                                { UnaryArithmeticOperator ( ( ~- ), e ) } %prec UMINUS
| LPAREN exp = expr RPAREN                                                                      { exp }
  
