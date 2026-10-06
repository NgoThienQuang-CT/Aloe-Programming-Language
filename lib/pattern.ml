open Option.Syntax

(** [split n acc lst] splits the [lst] at [n] index an return two lists. *)
let rec split n acc = function
  | xs when n <= 0 -> (List.rev acc, xs)
  | x :: xs -> split (n - 1) (x :: acc) xs
  | [] -> (List.rev acc, [])

let rec match_pattern pattern subject =
  match (pattern, subject) with
  | Ast.PatWildcard, _ -> Some []
  | Ast.PatVar var, _ -> Some [ (var, subject) ]
  | Ast.PatLit lit, _ -> match_literal_pattern lit subject
  | Ast.PatList patterns, Value.List list_subject ->
      match_list_pattern patterns list_subject
  | Ast.PatListRest (prev_patterns, rest), Value.List list_subject ->
      match_list_rest_pattern prev_patterns rest list_subject
  | Ast.PatMap patterns, Value.TreeMap map -> match_map_pattern patterns map
  | Ast.PatOr (lpattern, rpattern), subject -> match_or_pattern lpattern rpattern subject
  | _ -> None

and match_literal_pattern lit subject =
  match eval_literal_pattern lit with
  | Some v when Value.equal v subject -> Some []
  | _ -> None

and eval_literal_pattern = function
  | Ast.Number n -> Some (Value.Number n)
  | Ast.Unary (Neg, Ast.Number n) -> Some (Value.Number (-.n))
  | Ast.String s -> Some (Value.String s)
  | Ast.Boolean b -> Some (Value.Boolean b)
  | Ast.Nil -> Some Value.Nil
  | _ -> None

and match_list_pattern patterns list =
  match (patterns, list) with
  | [], [] -> Some []
  | p :: ps, v :: vs ->
      let* bindings = match_pattern p v in
      let* bindings' = match_list_pattern ps vs in
      Some (bindings @ bindings')
  | _ -> None

and match_list_rest_pattern prev_patterns rest list =
  let prev_len = List.length prev_patterns in
  let full_len = List.length list in
  if full_len < prev_len then None
  else
    let prev_list, rest_list = split prev_len [] list in
    let* bindings = match_list_pattern prev_patterns prev_list in
    match rest with
    | Some var -> Some (bindings @ [ (var, Value.List rest_list) ])
    | None -> Some bindings

and match_map_pattern patterns map =
  let rec aux acc = function
    | [] -> Some acc
    | (key_expr, val_pattern) :: entries ->
        let* vkey = eval_literal_pattern key_expr in
        let* vval = Value.MapOps.get vkey map in
        let* bindings = match_pattern val_pattern vval in
        aux (acc @ bindings) entries
  in
  aux [] patterns

and match_or_pattern lpattern rpattern subject =
  match match_pattern lpattern subject with
  | Some bindings -> Some bindings
  | None -> match_pattern rpattern subject
