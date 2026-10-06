type delimiter =
  | Curly
  | Paren

type state =
  | Normal
  | DQuote
  | Escape
  | Comment

type context =
  { stack : delimiter list;
    state : state
  }

let empty_ctx = { stack = []; state = Normal }
let push delim ctx = { ctx with stack = delim :: ctx.stack }

let pop ctx =
  match ctx.stack with
  | [] -> ctx
  | _ :: t -> { ctx with stack = t }

let change_state state ctx = { ctx with state }

let transition ctx ch =
  match ctx.state with
  | Normal ->
      begin match ch with
      | '"' -> change_state DQuote ctx
      | '#' -> change_state Comment ctx
      | '(' -> push Paren ctx
      | '{' -> push Curly ctx
      | ')' ->
          begin match ctx.stack with
          | Paren :: _ -> pop ctx
          | _ -> ctx
          end
      | '}' ->
          begin match ctx.stack with
          | Curly :: _ -> pop ctx
          | _ -> ctx
          end
      | _ -> ctx
      end
  | DQuote ->
      begin match ch with
      | '"' -> change_state Normal ctx
      | '\\' -> change_state Escape ctx
      | _ -> ctx
      end
  | Escape -> change_state DQuote ctx
  | Comment ->
      begin match ch with
      | '\n' -> change_state Normal ctx
      | _ -> ctx
      end

let is_done ctx = ctx.stack = [] && (ctx.state = Normal || ctx.state = Comment)
let check str ctx = String.fold_left transition ctx str
