type identifier = string ref;;

type expr = 
  | Integer of int
  | Variable of identifier
  | Field of expr * identifier
  | Null
  | Procedure of identifier * identifier * cmd
  | Call of expr * expr
  | BinaryArithmeticOperator of (int -> int -> int) * expr * expr
  | UnaryArithmeticOperator of (int -> int) * expr
and bool_expr =
  | Bool of bool
  | BoolExpr of expr
  | BinaryIntegerComparatorExpr of (int -> int -> bool) * expr * expr
  | UnaryIntegerComparatorExpr of (int -> bool) * expr
  | BinaryBoolComparatorExpr of (bool -> bool -> bool) * bool_expr * bool_expr
  | UnaryBoolComparatorExpr of (bool -> bool) * bool_expr
and cmd = 
  | Declaration of identifier
  | Assigment of identifier * expr
  | FieldAssignment of expr * identifier * expr
  | DeclarationAndAssigment of identifier * expr
  | CmdSequence of cmds
  | IfThenElseControlFlow of bool_expr * cmd * cmd
  | IfThenControlFlow of bool_expr * cmd
  | WhileControlFlow of bool_expr * cmd
  | ParallelFlow of cmd * cmd
  | AtomFlow of cmd
  | Malloc of identifier
  | Expression of expr
  | Debug of expr
  | Skip

and cmds = cmd list

and program = cmds;;