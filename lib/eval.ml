exception RuntimeError of string

let ( let* ) = Env.( let* )
let return = Env.return

let lookup name =
  let* opt_v = Env.lookup_opt name in
  match opt_v with
  | Some v -> return v
  | None ->
      let message = Printf.sprintf "NameError: variable %s is not defined" name in
      raise (RuntimeError message)

let rec eval expr =
  match expr with
  | Ast.Prog (exprs, trail) -> eval_prog exprs trail
  | Ast.Nil -> return Value.Nil
  | Ast.Ident name -> lookup name
  | Ast.Number num -> return (Value.Number num)
  | Ast.String str -> return (Value.String str)
  | Ast.Boolean bool -> return (Value.Boolean bool)
  | Ast.Func (params, fn_blk) ->
      let* fn_env = Env.get_env in
      return (Value.Closure { params; fn_blk; fn_env })
  | Ast.List (elems, rest) ->
      let* velems = Env.map_m eval elems in
      begin match rest with
      | None -> return (Value.List velems)
      | Some rest ->
          let* vrest = eval rest in
          begin match vrest with
          | Value.List vlist -> return (Value.List (velems @ vlist))
          | other ->
              let name = Value.name_of_value other in
              let message = Printf.sprintf "TypeError: spread expects a list, but got '%s'" name in
              raise (RuntimeError message)
          end
      end
  | Ast.Map pairs ->
      let* vpairs =
        Env.map_m
          (fun (k, v) ->
            let* k' = eval k in
            let* v' = eval v in
            return (k', v') )
          pairs
      in
      begin try return (Value.TreeMap (Value.MapOps.of_list vpairs)) with
      | Value.InvalidKey msg -> raise (RuntimeError msg)
      end
  | Ast.Unary (op, rhs) -> eval_unop op rhs
  | Ast.Binary (Ast.Assign, Ident name, rhs) -> eval_assign name rhs
  | Ast.Binary (Ast.Pipe, lhs, rhs) -> eval_pipe lhs rhs
  | Ast.Binary (op, lhs, rhs) -> eval_binop op lhs rhs
  | Ast.Block (exprs, trail) -> eval_block exprs trail
  | Ast.Call (func, args) -> apply func args
  | Ast.Index (target, index) -> eval_index target index
  | Ast.Match (subject, arms) -> eval_match subject arms

and eval_prog exprs trail =
  let* () = Env.iter_m eval exprs in
  match trail with
  | Some expr -> eval expr
  | None -> return Value.Nil

and eval_unop op rhs =
  let* v = eval rhs in
  return
    begin match (op, v) with
    | Ast.Not, Value.Boolean b -> Value.Boolean (not b)
    | Ast.Neg, Value.Number n -> Value.Number ~-.n
    | _ ->
        let op_str = Ast.string_of_un_op op in
        let v_name = Value.name_of_value v in
        let message =
          Printf.sprintf "TypeError: operator %s cannot apply to type %s" op_str v_name
        in
        raise (RuntimeError message)
    end

and eval_assign name rhs =
  match rhs with
  | Ast.Func (params, fn_blk) ->
      let* outer_env = Env.get_env in
      let rec func =
        (* Make func include itself in the environment to support recursion. *)
        let fn_env = (name, func) :: outer_env in
        Value.Closure { params; fn_blk; fn_env }
      in
      let* () = Env.extend_env name func in
      return func
  | other ->
      let* v = eval other in
      let* () = Env.extend_env name v in
      return v

and eval_pipe lhs rhs =
  let call =
    match rhs with
    | Ast.Call (func, args) -> Ast.Call (func, lhs :: args)
    | other -> Ast.Call (other, [ lhs ])
  in
  eval call

and eval_binop op lhs rhs =
  let* vl = eval lhs in
  let* vr = eval rhs in
  return
    begin match (op, vl, vr) with
    | Ast.Add, Value.Number a, Value.Number b -> Value.Number (a +. b)
    | Ast.Add, Value.String a, Value.String b -> Value.String (a ^ b)
    | Ast.Sub, Value.Number a, Value.Number b -> Value.Number (a -. b)
    | Ast.Mul, Value.Number a, Value.Number b -> Value.Number (a *. b)
    | Ast.Div, Value.Number _, Value.Number 0.0 ->
        raise (RuntimeError "ArithmeticError: division by zero")
    | Ast.Div, Value.Number a, Value.Number b -> Value.Number (a /. b)
    | Ast.Rem, Value.Number a, Value.Number b -> Value.Number (Float.rem a b)
    | Ast.Lss, Value.Number a, Value.Number b -> Value.Boolean (a < b)
    | Ast.Gtr, Value.Number a, Value.Number b -> Value.Boolean (a > b)
    | Ast.Leq, Value.Number a, Value.Number b -> Value.Boolean (a <= b)
    | Ast.Geq, Value.Number a, Value.Number b -> Value.Boolean (a >= b)
    | Ast.Lss, Value.String a, Value.String b -> Value.Boolean (a < b)
    | Ast.Gtr, Value.String a, Value.String b -> Value.Boolean (a > b)
    | Ast.Leq, Value.String a, Value.String b -> Value.Boolean (a <= b)
    | Ast.Geq, Value.String a, Value.String b -> Value.Boolean (a >= b)
    | Ast.Eql, _, _ -> Value.Boolean (Value.equal vl vr)
    | Ast.Neq, _, _ -> Value.Boolean (not (Value.equal vl vr))
    | Ast.And, Value.Boolean a, Value.Boolean b -> Value.Boolean (a && b)
    | Ast.Or, Value.Boolean a, Value.Boolean b -> Value.Boolean (a || b)
    | _ ->
        let op_str = Ast.string_of_bin_op op in
        let vl_name = Value.name_of_value vl in
        let vr_name = Value.name_of_value vr in
        let message =
          Printf.sprintf "TypeError: operator %s cannot apply to type %s and %s" op_str vl_name
            vr_name
        in
        raise (RuntimeError message)
    end

and eval_block exprs trail =
  let* env = Env.get_env in
  begin
    let* () = Env.iter_m eval exprs in
    match trail with
    | Some expr -> eval expr
    | None -> return Value.Nil
  end
  |> Env.with_local_env env

and apply func args =
  let* vfunc = eval func in
  let* vargs = Env.map_m eval args in
  match vfunc with
  | Closure closure ->
      let call_env = param_bind closure.params vargs @ closure.fn_env in
      eval closure.fn_blk |> Env.with_local_env call_env
  | Builtin builtin -> return (builtin.func vargs)
  | _ -> raise (RuntimeError "TypeError: cannot call a non-function value")

and param_bind params values =
  match (params, values) with
  (* Function has no parameters, and no arguments are provided. *)
  | [], [] -> []
  (* Function has no parameters, and some arguments are provided. Raise error. *)
  | [], _ :: _ -> raise (RuntimeError "TypeError: too many arguments provided")
  (* Function has some parameters, and no arguments are provided. *)
  | p :: _, [] -> raise (RuntimeError ("TypeError: missing required argument: " ^ p))
  (* Function has some parameters, and some arguments are provided. Bind this param and rescurse the
     rest. *)
  | p :: ps', v :: vs' -> (p, v) :: param_bind ps' vs'

and eval_index target index =
  let* vtarget = eval target in
  let* vindex = eval index in
  match vtarget with
  | Value.List list -> eval_list_index list vindex
  | Value.String str -> eval_string_index str vindex
  | Value.TreeMap map -> eval_map_index map vindex
  | _ ->
      let vtarget_name = Value.string_of_value vtarget in
      let message = Printf.sprintf "TypeError: type %s is not indexable" vtarget_name in
      raise (RuntimeError message)

and eval_list_index list = function
  | Value.Number n when Float.is_integer n ->
      let idx = int_of_float n in
      let len = List.length list in
      (* Support negative indexing *)
      let idx = if idx < 0 then len + idx else idx in
      if 0 <= idx && idx < len then return (List.nth list idx) else return Value.Nil
  | Value.Number _ -> raise (RuntimeError "TypeError: list index must be an integer")
  | other ->
      let vindex_name = Value.string_of_value other in
      let message = Printf.sprintf "TypeError: list index must be a number, get %s" vindex_name in
      raise (RuntimeError message)

and eval_string_index str = function
  | Value.Number n when Float.is_integer n ->
      let idx = int_of_float n in
      let len = String.length str in
      (* Support negative indexing *)
      let idx = if idx < 0 then len + idx else idx in
      if 0 <= idx && idx < len then return (Value.String (String.sub str idx 1))
      else return Value.Nil
  | Value.Number _ -> raise (RuntimeError "TypeError: string index must be an integer")
  | other ->
      let vindex_name = Value.string_of_value other in
      let message = Printf.sprintf "TypeError: string index must be a number, get %s" vindex_name in
      raise (RuntimeError message)

and eval_map_index map key =
  try
    match Value.MapOps.get key map with
    | None -> return Value.Nil
    | Some v -> return v
  with
  | Value.InvalidKey msg -> raise (RuntimeError msg)

and eval_match subject arms =
  let* vsubject = eval subject in
  let* env = Env.get_env in
  let rec eval_arms (arms : Ast.arm list) =
    match arms with
    | [] -> return Value.Nil
    | arm :: arms' -> (
        match Pattern.match_pattern arm.pat vsubject with
        | None -> eval_arms arms'
        | Some bindings ->
            let arm_env = bindings @ env in
            let* res = eval_arm arm |> Env.with_local_env arm_env in
            begin match res with
            | None -> eval_arms arms'
            | Some v -> return v
            end )
  in
  eval_arms arms

and eval_arm (arm : Ast.arm) =
  let* ok = guard_check arm.guard in
  if ok then
    let* res = eval arm.res in
    return (Some res)
  else return None

and guard_check guard =
  match guard with
  | None -> return true
  | Some guard -> (
      let* vguard = eval guard in
      match vguard with
      | Value.Boolean b -> return b
      | _ ->
          let vguard_name = Value.name_of_value vguard in
          let message =
            Printf.sprintf "TypeError: match guard must evaluate to a boolean, but got %s"
              vguard_name
          in
          raise (RuntimeError message) )
