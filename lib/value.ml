type t =
  | Number of float
  | String of string
  | Boolean of bool
  | Closure of closure
  | Builtin of builtin
  | List of t list
  | TreeMap of t tree
  | Nil

and closure =
  { params : string list;
    fn_blk : Ast.expr;
    fn_env : t Env.t
  }

and builtin =
  { name : string;
    func : t list -> t
  }

and 'v tree =
  | Empty
  | Node of
      { k : t;
        v : 'v;
        l : 'v tree;
        r : 'v tree;
        h : int
      }

exception InvalidKey of string

module MapOps = struct
  let empty = Empty

  let height = function
    | Empty -> 0
    | Node { h; _ } -> h

  let make k v l r = Node { k; v; l; r; h = 1 + max (height l) (height r) }

  let balance_factor = function
    | Empty -> 0
    | Node { l; r; _ } -> height l - height r

  let rotate_r = function
    | Node { k = ka; v = va; l = Node { k = kb; v = vb; l = lb; r = rb; _ }; r = ra; _ } ->
        make kb vb lb (make ka va rb ra)
    | tree -> tree

  let rotate_l = function
    | Node { k = ka; v = va; l = la; r = Node { k = kb; v = vb; l = lb; r = rb; _ }; _ } ->
        make kb vb (make ka va la lb) rb
    | tree -> tree

  let balance tree =
    let bf = balance_factor tree in
    if bf > 1 then
      match tree with
      | Node ({ l; _ } as n) when balance_factor l < 0 -> Node { n with l = rotate_l l } |> rotate_r
      | _ -> rotate_r tree
    else if bf < -1 then
      match tree with
      | Node ({ r; _ } as n) when balance_factor r > 0 -> Node { n with r = rotate_r r } |> rotate_l
      | _ -> rotate_l tree
    else tree

  let rec compare a b =
    match (a, b) with
    | Number x, Number y -> Float.compare x y
    | String x, String y -> String.compare x y
    | Boolean x, Boolean y -> Bool.compare x y
    | Boolean _, (Number _ | String _) -> -1
    | (Number _ | String _), Boolean _ -> 1
    | Number _, String _ -> -1
    | String _, Number _ -> 1
    | (List _ | TreeMap _), _
    | _, (List _ | TreeMap _) ->
        raise (InvalidKey "TypeError: composite types cannot be used as map keys")
    | (Closure _ | Builtin _), _
    | _, (Closure _ | Builtin _) ->
        raise (InvalidKey "TypeError: functions cannot be used as map keys")
    | Nil, _
    | _, Nil ->
        raise (InvalidKey "TypeError: nil cannot be used as map keys")

  let to_list tree =
    let rec aux acc = function
      | Empty -> acc
      | Node { k; v; l; r; _ } -> aux ((k, v) :: aux acc r) l
    in
    aux [] tree

  let rec get k = function
    | Empty -> None
    | Node { k = k'; v; l; r; _ } ->
        let d = compare k k' in
        if d = 0 then Some v else if d < 0 then get k l else get k r

  let rec set k v = function
    | Empty ->
        (* use compare to check invalid key *)
        compare k k |> ignore;
        Node { k; v; l = Empty; r = Empty; h = 1 }
    | Node ({ k = k'; l; r; _ } as n) ->
        let d = compare k k' in
        if d = 0 then Node { n with v }
        else if d < 0 then balance (make k' n.v (set k v l) r)
        else balance (make k' n.v l (set k v r))

  let rec remove_min = function
    | Empty -> invalid_arg "MapOps.remove_min: empty tree"
    | Node { k; v; l = Empty; r; _ } -> (k, v, r)
    | Node { k; v; l; r; _ } ->
        let min_k, min_v, l' = remove_min l in
        (min_k, min_v, balance (make k v l' r))

  let rec remove k = function
    | Empty ->
        (* use compare to check invalid key *)
        compare k k |> ignore;
        Empty
    | Node ({ k = k'; v; l; r; _ } as n) -> (
        let d = compare k k' in
        if d < 0 then balance (make k' n.v (remove k l) r)
        else if d > 0 then balance (make k' n.v l (remove k r))
        else
          match (l, r) with
          | Empty, _ -> r
          | _, Empty -> l
          | _, _ ->
              let min_k, min_v, r' = remove_min r in
              balance (make min_k min_v l r') )

  let rec of_list entries = List.fold_left (fun acc (k, v) -> set k v acc) Empty entries

  let rec fold f tree acc =
    match tree with
    | Empty -> acc
    | Node { k; v; l; r; _ } -> acc |> fold f l |> f k v |> fold f r

  let rec len = function
    | Empty -> 0
    | Node { l; r; _ } -> 1 + len r + len l
end

let rec equal a b =
  match (a, b) with
  | Number x, Number y -> Float.equal x y
  | String x, String y -> String.equal x y
  | Boolean x, Boolean y -> Bool.equal x y
  | Nil, Nil -> true
  | List x, List y -> List.equal equal x y
  | TreeMap x, TreeMap y ->
      List.equal
        (fun (k, v) (k', v') -> MapOps.compare k k' = 0 && equal v v')
        (MapOps.to_list x) (MapOps.to_list y)
  | Closure x, Closure y -> x == y
  | Builtin x, Builtin y -> x.name = y.name
  | _ -> false

let name_of_value = function
  | Number _ -> "number"
  | String _ -> "string"
  | Boolean _ -> "boolean"
  | Closure _ -> "function"
  | Builtin _ -> "built-in"
  | List _ -> "list"
  | TreeMap _ -> "map"
  | Nil -> "nil"

let rec string_of_value = function
  | Number n -> Printf.sprintf "%.16g" n
  | String s -> Printf.sprintf "%S" s
  | Boolean b -> string_of_bool b
  | Closure _ -> "<function>"
  | Builtin b -> Printf.sprintf "<built-in> : %s" b.name
  | List list -> list |> List.map string_of_value |> String.concat ", " |> Printf.sprintf "[%s]"
  | TreeMap tree ->
      MapOps.fold
        (fun k v acc ->
          let key_str =
            match k with
            | String s -> Printf.sprintf "%S" s
            | other -> string_of_value other
          in
          Printf.sprintf "%s: %s" key_str (string_of_value v) :: acc )
        tree []
      |> List.rev |> String.concat ", " |> Printf.sprintf "%%{%s}"
  | Nil -> "nil"
