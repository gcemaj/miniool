open Printf;;
open Ast;;
open SemanticDomains;;

exception EvaluationError of string;;

let heap_counter = ref 0;;
let global_stack = ref((Hashtbl.create 100));;
let heap = ref((Hashtbl.create 100));;

let rec print_t_value t_value = match t_value with
  | IntValue i -> printf "Int Value -> %d\n%!" i
  | Closure clo -> printf "Closure -> arg: %s\n%!" !clo.arg_in
  | Error err -> printf "Error -> %s\n%!" err
  | NullValue -> printf "Null\n%!"
  | ObjValue (_, t_val) -> printf "Obj Value -> "; print_t_value t_val;;

let get_next_heap_counter () = 
  let curr_counter = !heap_counter in
  heap_counter := !heap_counter + 1;
  Object curr_counter;;

let push_value_for_ident ident value stack heap = 
    let next_location = get_next_heap_counter() in
    Hashtbl.add !stack ident next_location;
    Hashtbl.add !heap (next_location, "value") value;;

let get_value_from_ident ident field stack heap = match Hashtbl.find_opt !stack ident with
  | None -> Error (sprintf "No %s in scope" ident)
  | Some location -> Hashtbl.find !heap (location, field);;

let set_value_for_ident ident value stack heap = 
  match Hashtbl.find_opt !stack ident with
    | None -> push_value_for_ident ident value stack heap
    | Some location -> Hashtbl.add !heap (location, "value") value;; 

let pop_value_for_ident ident stack = Hashtbl.remove !stack ident;;

let rec eval_expr expr stack heap = match expr with
  | Integer i -> IntValue i 
  | Variable id -> get_value_from_ident !id "value" stack heap
  | Field (exp, field) -> (
    let e1 = eval_expr exp stack heap in 
    match e1 with
      | ObjValue (loc, _) -> (Hashtbl.find !heap (loc, !field))
      | _ -> Error "Variable has not been malloc'ed"
    )
  | Null -> NullValue
  | Procedure (arg_in, arg_out, cmd) -> Closure (ref {arg_in = !arg_in; arg_out= !arg_out; cmd = cmd; stack = ( (Hashtbl.copy !stack)) } )
  | Call (expr1, expr2) -> (
      let e1 = (eval_expr expr1 stack heap) in
      let e2 = (eval_expr expr2 stack heap) in
      match e1 with
        | Closure clo -> (
            let curr_stack = ref (Hashtbl.copy !clo.stack) in
            push_value_for_ident !clo.arg_in e2 curr_stack heap;
            push_value_for_ident !clo.arg_out NullValue curr_stack heap;
            eval_cmds [!clo.cmd] curr_stack heap;
            get_value_from_ident !clo.arg_out "value" curr_stack heap
          )
        | _ -> Error "Cannot call on expression that is not a procedure"
    )
  | BinaryArithmeticOperator (op, lhs, rhs) -> 
    (
      let eval_lhs = (eval_expr lhs stack heap) in
      let eval_rhs =  (eval_expr rhs stack heap) in 
      match eval_lhs, eval_rhs with
        | IntValue e1, IntValue e2  -> (
            try 
              let result = (op e1 e2) in IntValue result
            with Division_by_zero -> Error (sprintf "Division By Zero num:%d denom:%d" e1 e2)
          )
        | Error err1, Error err2 -> Error (sprintf "%s and %s" err1 err2)
        | Error err, _ | _, Error err -> Error err
        | _, _ -> Error "Cannot perform operation on values that are not of type Interger" 
    )
  | UnaryArithmeticOperator (op, lhs) ->
    (
      let eval_lhs = (eval_expr lhs stack heap) in
      match eval_lhs with
        | IntValue e1 -> IntValue (op e1)
        | _ -> Error "Cannot perform operation on values that are not of type Interger" 
    )
and eval_bool_expr bool_expr stack heap = match bool_expr with
  | Bool b -> b
  | BoolExpr exp -> (
      let eval_exp = (eval_expr exp stack heap) in
      match eval_exp with
        | IntValue e1 -> if e1 == 0 then false else true
        | NullValue -> false
        | ObjValue _ -> raise (EvaluationError "Object has no concept of truthyness")
        | Closure _ -> raise (EvaluationError "Closure has no concept of truthyness")
        | Error err -> raise (EvaluationError err)
    )
  | BinaryIntegerComparatorExpr (op, lhs, rhs) -> (
      let eval_lhs = (eval_expr lhs stack heap) in
      let eval_rhs = (eval_expr rhs stack heap) in
      match eval_lhs, eval_rhs with
        | IntValue e1, IntValue e2 -> (op e1 e2)
        | Error err1 , Error err2 -> raise (EvaluationError (sprintf "%s and %s" err1 err2))
        | _, Error err | Error err, _ -> raise (EvaluationError err)
        | _, _ -> raise (EvaluationError "Cannot perform operation on values that are not of type Interger ")
    )
  | UnaryIntegerComparatorExpr (op, lhs) -> (
      let eval_lhs = (eval_expr lhs stack heap) in
      match eval_lhs with
        | IntValue e1 -> (op e1)
        | Error err -> raise (EvaluationError err)
        | _ -> raise (EvaluationError "Cannot perform operation on values that are not of type Interger")
    )
  | BinaryBoolComparatorExpr (op, lhs, rhs) -> (op (eval_bool_expr lhs stack heap) (eval_bool_expr rhs stack heap))
  | UnaryBoolComparatorExpr (op, lhs )-> (op (eval_bool_expr lhs stack heap))
and eval_cmd cmd stack heap = 
  match cmd with
  | Declaration (id) -> set_value_for_ident !id NullValue stack heap; []
  | DeclarationAndAssigment (id, expr) | Assigment (id, expr) -> (
      let e1 = (eval_expr expr stack heap) in
      set_value_for_ident !id e1 stack heap;
      match e1 with
        | Closure clo -> (push_value_for_ident !id e1 (ref (!clo.stack)) heap);[]
        | Error err -> raise (EvaluationError err)
        | _ -> []
    )
  | FieldAssignment (expr1, field, expr2) -> (
      let e1 = (eval_expr expr1 stack heap) in
      let e2 = (eval_expr expr2 stack heap) in
      match e1 with
        | ObjValue (loc, _) -> Hashtbl.add !heap (loc, !field) e2;[]
        | _ -> raise (EvaluationError "Cannot assign field to variable that has not been malloc'ed")
    )
  | Malloc id -> (
      let loc = Hashtbl.find !stack !id in
      let heap_val = Hashtbl.find !heap (loc, "value") in
      match heap_val with
        | NullValue -> Hashtbl.add !heap (loc, "value") (ObjValue (loc, NullValue));[]
        | _ -> raise (EvaluationError "Can only apply malloc to variable with NullValue")
    )
  | CmdSequence (cmds) -> (
      match cmds with
      | [] -> []
      | h::t -> (eval_cmd h stack heap) @ t
    )
  | IfThenElseControlFlow (bool_expr, if_cmd, else_cmd) -> if (eval_bool_expr bool_expr stack heap) then [if_cmd] else [else_cmd]
  | IfThenControlFlow (bool_expr, if_cmd) -> if (eval_bool_expr bool_expr stack heap) then [if_cmd] else []
  | WhileControlFlow (bool_expr, while_cmd) -> if (eval_bool_expr bool_expr stack heap) then [while_cmd;cmd] else []
  | AtomFlow (c) -> eval_cmds [c] stack heap; []
  | ParallelFlow (c1, c2) -> (
      match Random.bool () with
        | true -> (
            match eval_cmd c1 stack heap with
              | [] -> [c2]
              | h::t -> [ParallelFlow (CmdSequence (h::t), c2)]
          )
        | false -> ( 
            match eval_cmd c2 stack heap with
              | [] -> [c1]
              | h::t ->  [ParallelFlow (CmdSequence (h::t), c1)]
          )
    )
  | Expression (expr) -> (
      let e = (eval_expr expr stack heap) in
      match e with
        | Error err -> raise (EvaluationError err)
        | _ -> []
    )
  | Debug (expr) -> print_t_value (eval_expr expr stack heap); []
  | Skip -> []
and eval_cmds cmds stack heap = 
  match cmds with
  | [] -> ();
  | h::t -> eval_cmds ((eval_cmd h stack heap) @ t) stack heap;;

let eval_ast ast = eval_cmds ast global_stack heap;