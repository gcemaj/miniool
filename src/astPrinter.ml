open Printf;;
open Ast;;

let print_binary_bool_op op = match (op true false) with
    true -> "||"
  | false -> "&&"

let print_binary_arithmetic_op op = match (op 10 5) with
    15 -> "+"
  | 5 -> "-"
  | 50 -> "*"
  | 2 -> "/"
  | _ -> "ERROR"

let print_binary_int_comp_op op = match (op 1 2) with
    true -> (match (op 1 1) with
        true -> "<="
      | false -> "<" ) 
  | false -> ( match (op 2 1)  with 
      true -> (match (op 1 1 ) with
        true -> ">="
      | false-> ">")
    | false -> "==");;

let rec print_expr exp = match exp with
    Integer i -> sprintf "Integer(value=%d)" i
  | BinaryArithmeticOperator (op, lhs, rhs) -> ""
  | UnaryArithmeticOperator (op, lhs) -> ""
  | Variable id -> sprintf "Variable(name=%s, value=??, op=Get())" !id

let rec print_bool_expr bool_exp tabs = match bool_exp with
  | Bool b -> sprintf "Value(type=Bool, value=%B)" b
  | BinaryIntegerComparatorExpr (op, lhs, rhs) -> sprintf "Comparator(\n\t%slhs=%s, \n\t%srhs=%s, \n\t%sop=(%s)\n%s)" tabs (print_expr lhs) tabs (print_expr rhs) tabs (print_binary_int_comp_op op) tabs
  | UnaryIntegerComparatorExpr (op, lhs) -> ""
  | BinaryBoolComparatorExpr (op, lhs, rhs) -> sprintf "Comparator(lhs=%s, rhs=%s, op=(%s))" (print_bool_expr lhs (sprintf "\t%s" tabs)) (print_bool_expr rhs (sprintf "\t%s" tabs)) (print_binary_bool_op op)
  | UnaryBoolComparatorExpr (op, lhs )-> "";;

let print_cmd cmd tabs = match cmd with
    Declaration (id) -> printf "%sVariable(name=%s, value=NULL, op=Create())" tabs !id
  | Assigment (id, _) -> printf "%sVariable(name=%s, value=NULL, op=Assign())" tabs !id
  | DeclarationAndAssigment (id, _) -> printf "%sVariable(name=%s, value=NULL, op=CreateAndAssign())" tabs !id
  | IfThenElseControlFlow (bool_exp, if_cmd, else_cmd) -> printf "%sIfElse(cond=%s, true_body=NULL, false_body=NULL)" tabs (print_bool_expr bool_exp (sprintf "\t%s" tabs))
  | IfThenControlFlow (bool_exp, if_cmd) -> printf "%sIfElse(\n\tcond=%s, \n\ttrue_body=NULL, \n\tfalse_body=NULL\n)" tabs (print_bool_expr bool_exp (sprintf "\t%s" tabs))
  | WhileControlFlow (bool_exp, if_cmd) -> printf "%sWhile(cond=%s, true_body=NULL, false_body=NULL)" tabs (print_bool_expr bool_exp (sprintf "\t%s" tabs))
  | Expression (exp) -> printf "%sExpression(%s)" tabs (print_expr exp)

let rec print_cmds cmds tabs = match cmds with
    h::t -> (print_cmd h tabs); print_newline(); print_cmds t tabs;
  | [] -> ();;

let print_ast ast = print_cmds ast ""