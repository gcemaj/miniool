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
  | 0 -> "%"
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
  | Integer i -> sprintf "Integer(value=%d)" i
  | Variable id ->sprintf "Variable(name=%s)" !id
  | Field (exp, field) -> sprintf "Filed(value=%s, field=%s)" (print_expr exp) !field
  | Null -> "null"
  | Procedure (arg_in, arg_out, cmd) -> sprintf "Procedure(arg_in=%s, arg_out=%s, body=%s)" !arg_in !arg_out (print_cmd cmd)
  | Call (expr1, expr2) -> sprintf "Call(proc=%s, arg_in=%s)" (print_expr expr1) (print_expr expr2)
  | BinaryArithmeticOperator (op, lhs, rhs) -> sprintf "BinaryOp(lhs=%s, op=%s, rhs=%s)" (print_expr lhs) (print_binary_arithmetic_op op) (print_expr rhs)
  | UnaryArithmeticOperator (op, lhs) -> sprintf "UnaryOp(lhs=%s, op=-)" (print_expr lhs)
and print_bool_expr bool_exp = match bool_exp with
  | Bool b -> sprintf "Bool(value=%b)" b
  | BoolExpr exp -> sprintf "BoolExpr(value=%s)" (print_expr exp)
  | BinaryIntegerComparatorExpr (op, lhs, rhs) -> sprintf "BinaryIntComp(lhs=%s, op=%s, rhs=%s)" (print_expr lhs) (print_binary_int_comp_op op) (print_expr rhs)
  | UnaryIntegerComparatorExpr (op, lhs) ->  sprintf "UnaryIntComp(lhs=%s, op=*)" (print_expr lhs)
  | BinaryBoolComparatorExpr (op, lhs, rhs) -> sprintf "BinaryBoolComp(lhs=%s, op=%s, rhs=%s)" (print_bool_expr lhs) (print_binary_bool_op op) (print_bool_expr rhs)
  | UnaryBoolComparatorExpr (op, lhs )-> sprintf "UnaryBoolComp(lhs=%s, op=~)" (print_bool_expr lhs)
and print_cmd cmd = match cmd with
  | Declaration (id) -> sprintf "VariableDecl(name=%s)" !id
  | DeclarationAndAssigment (id, expr) | Assigment (id, expr) -> sprintf "VariableAssign(name=%s, value=%s)" !id (print_expr expr)
  | FieldAssignment (expr1, field, expr2) -> sprintf "VariableAssign(name=%s, field=%s, value=%s)"(print_expr expr1) !field (print_expr expr2)
  | Malloc id -> sprintf "Malloc(name=%s)" !id
  | CmdSequence (cmds) -> sprintf "CmdSequence(cmds=[%s])" (print_cmds cmds)
  | IfThenElseControlFlow (bool_expr, if_cmd, else_cmd) -> sprintf "IfElse(cond=%s, if_clause=%s, else_clause=%s)" (print_bool_expr bool_expr) (print_cmd if_cmd) (print_cmd else_cmd)
  | IfThenControlFlow (bool_expr, if_cmd) ->  sprintf "IfElse(cond=%s, if_clause=%s, else_clause=Null)" (print_bool_expr bool_expr) (print_cmd if_cmd) 
  | WhileControlFlow (bool_expr, while_cmd) ->  sprintf "While(cond=%s, while_clause=%s)" (print_bool_expr bool_expr) (print_cmd while_cmd) 
  | AtomFlow (c) -> sprintf "Atom(cmd=%s)" (print_cmd c)
  | ParallelFlow (c1, c2) -> sprintf "Parallel(lhs=%s, rhs=%s)" (print_cmd c1) (print_cmd c2)
  | Expression (expr) -> sprintf "Expr(expr=%s)" (print_expr expr)
  | Debug (expr) -> sprintf "Debug(expr=%s)" (print_expr expr)
  | Skip -> "Skip()"
and print_cmds cmds = match cmds with
  | h::t -> (
      let printed_cmds = (print_cmds t) in
      match printed_cmds with
        | "" -> (print_cmd h)
        | _ -> sprintf "%s, %s" (print_cmd h) printed_cmds
    )
  | [] -> "";;

let print_ast ast = printf "%s\n%!" (print_cmds ast); ()