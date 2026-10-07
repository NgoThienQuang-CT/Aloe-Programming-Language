type 'a t = (string * 'a) list

let empty : 'a t = []

type ('a, 'res) state = 'a t -> 'res * 'a t

(** [return x] sets the result to [x] without changing env. *)
let return (x : 'res) : ('a, 'res) state = fun env -> (x, env)

(** [bind m f] executes [m] with env then threads the result to [f]. *)
let bind (m : ('a, 'res1) state) (f : 'res1 -> ('a, 'res2) state) : ('a, 'res2) state =
 fun env ->
  let v, nxt_env = m env in
  f v nxt_env

let ( let* ) = bind

(** [get_env] sets the result to env without changing env. *)
let get_env : ('a, 'a t) state = fun env -> (env, env)

let get = get_env

(** [set_env new_env] sets the result to unit and sets the env to [new_env]. *)
let set_env (new_env : 'a t) : ('a, unit) state = fun _ -> ((), new_env)

let set = set_env

(** [modify_env f] sets the result to unit and sets the env to [f env]. *)
let modify_env (f : 'a t -> 'a t) : ('a, unit) state = fun env -> ((), f env)

let modify = modify_env

(** [with_local_env env m] runs [m] inside [env], then discards [env] when [m] finishes to preserve
    outer environment. *)
let with_local_env (env : 'a t) (m : ('a, 'res) state) : ('a, 'res) state =
 fun outer_env ->
  let v, _ = m env in
  (v, outer_env)

let with_local = with_local_env

(** [extend_env name value] adds a new binding of [name] and [value]. *)
let extend_env name value : ('a, unit) state = modify_env (fun env -> (name, value) :: env)

let extend = extend_env

(** [lookup_opt name] looks up a variable in env and returns option result. *)
let lookup_opt name : ('a, 'a option) state =
  let* env = get_env in
  return (List.assoc_opt name env)

let rec map_m f = function
  | [] -> return []
  | x :: xs ->
      let* y = f x in
      let* ys = map_m f xs in
      return (y :: ys)

let rec iter_m f = function
  | [] -> return ()
  | x :: xs ->
      let* _ = f x in
      iter_m f xs

let rec fold_left_m f acc = function
  | [] -> return acc
  | x :: xs ->
      let* acc' = f acc x in
      fold_left_m f acc' xs
