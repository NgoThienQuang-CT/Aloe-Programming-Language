module L = MenhirLib.LexerUtil
module E = MenhirLib.ErrorReports
module I = Parser.MenhirInterpreter

let red = "\x1b[1;31m"
let blu = "\x1b[1;36m"
let wht = "\x1b[1;37m"
let clr = "\x1b[0m"
let max_source_lines = 6
let ( ||? ) str default = if String.length str = 0 then default else str
let pad n = String.make (max 0 n) ' '

let get_line lines n =
  match List.nth_opt lines (n - 1) with
  | Some s ->
      if not (String.ends_with ~suffix:"\r" s) then s
      else String.sub s 0 (String.length s - 1)
  | None -> ""

let error_info (span : Lexing.position * Lexing.position) =
  let s, e = span in
  let file = s.pos_fname ||? "<stdin>" in
  let beg_line = max 1 s.pos_lnum in
  let beg_col = max 1 (s.pos_cnum - s.pos_bol + 1) in
  let end_line = max beg_line e.pos_lnum in
  let end_col = max 1 (e.pos_cnum - e.pos_bol + 1) in
  let span_len = max 1 (e.pos_cnum - s.pos_cnum) in
  (file, beg_line, beg_col, end_line, end_col, span_len)

let report source (span : Lexing.position * Lexing.position) message =
  let message = String.trim message in
  let file, beg_line, beg_col, end_line, end_col, span_len = error_info span in
  let lines = String.split_on_char '\n' source in
  let max_line = max beg_line end_line in
  let line_number_size = max 3 (String.length (string_of_int max_line)) in
  let margin = pad line_number_size in
  let out = Buffer.create 256 in
  let write line =
    Buffer.add_string out line;
    Buffer.add_char out '\n'
  in

  (* ERR1, ERR2, ERR3 *)
  write (Printf.sprintf "%serror%s: %s%s%s" red clr wht message clr);
  write (Printf.sprintf "%s%s-->%s %s:%d:%d" margin blu clr file beg_line beg_col);
  write (Printf.sprintf "%s%s | %s" margin blu clr);

  if beg_line = end_line then begin
    let padding = pad (beg_col - 1) in
    let underline = String.make span_len '^' in
    let line_str = get_line lines beg_line in
    (* ERR4, ERR5 *)
    write (Printf.sprintf "%*d%s | %s%s" line_number_size beg_line blu clr line_str);
    write (Printf.sprintf "%s%s | %s%s%s%s%s" margin blu clr padding red underline clr)
  end
  else begin
    let number_of_lines = end_line - beg_line + 1 in
    let upper_arrow = String.make (max 0 (beg_col - 1)) '_' in
    (* ERR6, ERR7 *)
    write
      (Printf.sprintf "%*d%s | %s  %s" line_number_size beg_line blu clr
         (get_line lines beg_line) );
    write (Printf.sprintf "%s%s | %s%s _%s^%s" margin blu clr red upper_arrow clr);

    if number_of_lines <= max_source_lines then
      for l = beg_line + 1 to end_line - 1 do
        (* ERR8 *)
        write
          (Printf.sprintf "%*d%s | %s%s|%s %s" line_number_size l blu clr red clr
             (get_line lines l) )
      done
    else begin
      (* ERR8, ERR9, ERR8 *)
      write
        (Printf.sprintf "%*d%s | %s%s|%s %s" line_number_size (beg_line + 1) blu clr red
           clr
           (get_line lines (beg_line + 1)) );
      write (Printf.sprintf "%*s%s | %s%s|%s" line_number_size "..." blu clr red clr);
      write
        (Printf.sprintf "%*d%s | %s%s|%s %s" line_number_size (end_line - 1) blu clr red
           clr
           (get_line lines (end_line - 1)) )
    end;

    let lower_arrow = String.make (max 0 (end_col - 1)) '_' in
    (* ERR8, ERR10 *)
    write
      (Printf.sprintf "%*d%s | %s%s|%s %s" line_number_size end_line blu clr red clr
         (get_line lines end_line) );
    write (Printf.sprintf "%s%s | %s%s|_%s^%s" margin blu clr red lower_arrow clr)
  end;

  (* ERR3 *)
  write (Printf.sprintf "%s%s | %s" margin blu clr);

  let res = Buffer.contents out in
  if String.ends_with ~suffix:"\n" res then String.sub res 0 (String.length res - 1)
  else res

let rec loop source lexbuf = function
  | I.InputNeeded _ as checkpoint ->
      let token =
        try
          let tok = Lexer.read lexbuf in
          let beg_pos = lexbuf.lex_start_p in
          let end_pos = lexbuf.lex_curr_p in
          Ok (tok, beg_pos, end_pos)
        with
        | Lexer.SyntaxError { message; _ } ->
            let beg_pos = lexbuf.lex_start_p in
            let end_pos = lexbuf.lex_curr_p in
            Error (report source (beg_pos, end_pos) message)
      in
      begin match token with
      | Ok (tok, beg_pos, end_pos) ->
          let nxt_checkpoint = I.offer checkpoint (tok, beg_pos, end_pos) in
          loop source lexbuf nxt_checkpoint
      | Error err -> Error err
      end
  | (I.Shifting _ | I.AboutToReduce _) as checkpoint ->
      loop source lexbuf (I.resume checkpoint)
  | I.HandlingError env ->
      let span =
        try I.positions env with
        | Not_found -> (lexbuf.lex_start_p, lexbuf.lex_curr_p)
      in
      let message =
        try Parser_messages.message (I.current_state_number env) with
        | Not_found -> "Syntax error (unexpected token)"
      in
      Error (report source span message)
  | I.Accepted ast -> Ok ast
  | I.Rejected -> Error "Parser rejected the input."

let parse filename source =
  let lexbuf = L.init filename (Lexing.from_string source) in
  loop source lexbuf (Parser.Incremental.prog lexbuf.lex_curr_p)
