type location = 
  | Object of int
  | NullLoc
and closure = {arg_in: string; arg_out: string;  cmd: Ast.cmd; stack: stack}  
and t_value = 
  | IntValue of int
  | ObjValue of (location * t_value)
  | Closure of closure ref
  | Error of string
  | NullValue
and stack = (string, location) Hashtbl.t
and heap = Heap of (location*string, t_value) Hashtbl.t;;
