open Base
open Aloe

let kw_style =
  { LTerm_style.none with foreground = Some LTerm_style.lmagenta; bold = Some true }

let bool_style =
  { LTerm_style.none with foreground = Some LTerm_style.yellow; bold = Some true }

let num_style = { LTerm_style.none with foreground = Some LTerm_style.lyellow }
let str_style = { LTerm_style.none with foreground = Some LTerm_style.green }
let var_style = { LTerm_style.none with foreground = Some LTerm_style.lcyan }

let style_of_token = function
  | Parser.FN_KW
  | Parser.MATCH_KW
  | Parser.AND_KW
  | Parser.OR_KW ->
      Some kw_style
  | Parser.TRUE_KW
  | Parser.FALSE_KW ->
      Some bool_style
  | Parser.INT _
  | Parser.FLOAT _ ->
      Some num_style
  | Parser.STRING _ -> Some str_style
  | Parser.IDENT _ -> Some var_style
  | _ -> None

let highlight text =
  let lexbuf = text |> LTerm_text.to_string |> Zed_string.to_utf8 |> Lexing.from_string in
  let rec tokenize () =
    try
      let token = Lexer.read lexbuf in
      match token with
      | Parser.EOF -> ()
      | _ ->
          let beg_pos = lexbuf.lex_start_p.pos_cnum in
          let end_pos = lexbuf.lex_curr_p.pos_cnum in
          ( match style_of_token token with
          | Some style ->
              let len = Array.length text in
              for i = beg_pos to min (end_pos - 1) (len - 1) do
                let ch, style' = text.(i) in
                text.(i) <- (ch, LTerm_style.merge style style')
              done
          | None -> () );
          tokenize ()
    with
    | Lexer.SyntaxError { message; _ } ->
        if String.is_substring message ~substring:"string" then
          let beg_pos = lexbuf.lex_start_p.pos_cnum in
          let end_pos = lexbuf.lex_curr_p.pos_cnum in
          let len = Array.length text in
          for i = beg_pos to min (end_pos - 1) (len - 1) do
            let ch, style' = text.(i) in
            text.(i) <- (ch, LTerm_style.merge str_style style')
          done
  in
  try tokenize () with
  | _ -> ()
(* catch lex error to continue highlight *)
