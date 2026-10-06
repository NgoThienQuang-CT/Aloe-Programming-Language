let make name func = (name, Value.Builtin { name; func })

let error_arity name expected got =
  let arg_word = if expected = 1 then "argument" else "arguments" in
  let message =
    Printf.sprintf "TypeError: %s expected %d %s, but got %d" name expected arg_word got
  in
  raise (Eval.RuntimeError message)

let error_unsupported_type name v =
  let v_name = Value.name_of_value v in
  let message =
    Printf.sprintf "TypeError: %s was not supported for argument of type '%s'" name v_name
  in
  raise (Eval.RuntimeError message)

let error_argument_type name pos expected v =
  let pos_str =
    match pos with
    | 1 -> "first"
    | 2 -> "second"
    | 3 -> "third"
    | n -> string_of_int n ^ "th"
  in
  let v_name = Value.name_of_value v in
  let message =
    Printf.sprintf "TypeError: %s expects a %s as %s argument, but got '%s'" name expected
      pos_str v_name
  in
  raise (Eval.RuntimeError message)

let builtin_println = function
  | [ Value.String string ] ->
      print_endline string;
      Value.Nil
  | [ v ] ->
      print_endline (Value.string_of_value v);
      Value.Nil
  | args -> error_arity "println" 1 (List.length args)

let builtin_len = function
  | [ Value.String string ] -> Value.Number (string |> String.length |> float_of_int)
  | [ Value.List list ] -> Value.Number (list |> List.length |> float_of_int)
  | [ Value.TreeMap map ] -> Value.Number (map |> Value.MapOps.len |> float_of_int)
  | [ other ] -> error_unsupported_type "len" other
  | args -> error_arity "len" 1 (List.length args)

let builtin_to_string = function
  | [ Value.Number number ] -> Value.String (Printf.sprintf "%.16g" number)
  | [ Value.String string ] -> Value.String string
  | [ Value.Boolean boolean ] -> Value.String (string_of_bool boolean)
  | [ Value.Nil ] -> Value.String "nil"
  | [ other ] -> error_unsupported_type "to_string" other
  | args -> error_arity "to_string" 1 (List.length args)

let builtin_to_list = function
  | [ Value.String string ] ->
      let chars =
        List.init (String.length string) (fun i -> Value.String (String.sub string i 1))
      in
      Value.List chars
  | [ Value.List list ] -> Value.List list
  | [ Value.TreeMap map ] ->
      let pairs =
        map |> Value.MapOps.to_list |> List.map (fun (k, v) -> Value.List [ k; v ])
      in
      Value.List pairs
  | [ other ] -> error_unsupported_type "to_list" other
  | args -> error_arity "to_list" 1 (List.length args)

let builtin_type = function
  | [ v ] -> Value.String (Value.name_of_value v)
  | args -> error_arity "type" 1 (List.length args)

let builtin_load get_env = function
  | [ Value.String path ] ->
      let content =
        try In_channel.with_open_text path In_channel.input_all with
        | Sys_error message -> raise (Eval.RuntimeError ("IOError: " ^ message))
      in
      begin match Parse.parse path content with
      | Error message -> raise (Eval.RuntimeError message)
      | Ok ast ->
          let v, _ = Eval.eval ast (get_env ()) in
          v
      end
  | [ other ] ->
      let name = Value.name_of_value other in
      let message =
        Printf.sprintf "TypeError: load expects a string path, but got '%s'" name
      in
      raise (Eval.RuntimeError message)
  | args -> error_arity "load" 1 (List.length args)

let builtin_slice = function
  | [ Value.String string; Value.Number pos; Value.Number len ] ->
      let pos = int_of_float pos in
      let len = int_of_float len in
      let str_len = String.length string in
      let pos = if pos < 0 then pos + str_len else pos in
      if pos < 0 || len < 0 || pos + len > str_len then Value.Nil
      else Value.String (String.sub string pos len)
  | [ v1; v2; v3 ] ->
      begin match (v1, v2, v3) with
      | Value.String _, Value.Number _, other ->
          error_argument_type "slice" 3 "number" other
      | Value.String _, other, _ -> error_argument_type "slice" 2 "number" other
      | other, _, _ -> error_argument_type "slice" 1 "string" other
      end
  | args -> error_arity "slice" 3 (List.length args)

let builtin_cons = function
  | [ elem; Value.List list ] -> Value.List (elem :: list)
  | [ _; other ] -> error_argument_type "cons" 2 "list" other
  | args -> error_arity "cons" 2 (List.length args)

let builtin_put = function
  | [ Value.TreeMap map; key; value ] -> Value.TreeMap (Value.MapOps.set key value map)
  | [ other; _; _ ] -> error_argument_type "put" 1 "map" other
  | args -> error_arity "put" 3 (List.length args)

let builtin_delete = function
  | [ Value.TreeMap map; key ] -> Value.TreeMap (Value.MapOps.remove key map)
  | [ other; _ ] -> error_argument_type "delete" 1 "map" other
  | args -> error_arity "delete" 2 (List.length args)
