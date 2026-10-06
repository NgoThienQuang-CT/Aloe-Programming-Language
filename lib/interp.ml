let rec make_initial_env () =
  let open Builtins in
  [ make "println" builtin_println;
    make "len" builtin_len;
    make "type" builtin_type;
    make "to_string" builtin_to_string;
    make "load" (builtin_load make_initial_env);
    make "cons" builtin_cons;
    make "put" builtin_put;
    make "delete" builtin_delete;
    make "to_list" builtin_to_list;
    make "slice" builtin_slice
  ]

let initial_env = make_initial_env ()

let interp ?(filename = "<stdin>") input env =
  match Parse.parse filename input with
  | Error message -> (message, env)
  | Ok ast -> (
      try
        let v, env' = Eval.eval ast env in
        (Value.string_of_value v, env')
      with
      | Eval.RuntimeError message -> (message, env) )
