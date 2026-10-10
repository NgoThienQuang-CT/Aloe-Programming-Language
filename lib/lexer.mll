{
  open Lexing
  open Parser

  exception SyntaxError of {
    message : string;
    line : int;
    col : int;
  }

  let error message lexbuf =
    let pos = lexbuf.lex_curr_p in
    raise (SyntaxError { message; line = pos.pos_lnum; col = pos.pos_cnum - pos.pos_bol + 1 })

  let keywords = Hashtbl.create 53

  let _ =
    List.iter (fun (keyword, token) -> Hashtbl.add keywords keyword token)
    [
      ("and",   AND_KW);
      ("false", FALSE_KW);
      ("fn",    FN_KW);
      ("match", MATCH_KW);
      ("nil",   NIL_KW);
      ("or",    OR_KW);
      ("true",  TRUE_KW);
      ("use",   USE_KW);
      ("when",  WHEN_KW);
    ]
}

let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z' '_']
let ident = letter (letter | digit)*

rule read =
  parse
  | "."  { DOT }
  | ","  { COMMA }
  | ":"  { COLON }
  | ";"  { SEMICOLON }
  | "("  { LPAREN }
  | ")"  { RPAREN }
  | "{"  { LCURLY }
  | "}"  { RCURLY }
  | "["  { LBRACK }
  | "]"  { RBRACK }
  | "+"  { ADD }
  | "-"  { SUB }
  | "*"  { MUL }
  | "/"  { DIV }
  | "%"  { REM }
  | "="  { ASSIGN }
  | "!"  { NOT }
  | "<"  { LSS }
  | ">"  { GTR }
  | "|"  { BAR }
  | "&"  { AMPERSAND }
  | "@"  { ATSIGN }
  | ":=" { WALRUS }
  | "==" { EQL }
  | "!=" { NEQ }
  | "<=" { LEQ }
  | ">=" { GEQ }
  | "->" { RARROW }
  | "<-" { LARROW }
  | "|>" { PIPE }
  | "%{" { PERCENT_LCURLY }
  | ".." { DOTDOT }

  | '"' {
    let beg_pos = lexbuf.lex_start_p in
    try
      let token = read_string (Buffer.create 32) lexbuf in
      lexbuf.lex_start_p <- beg_pos;
      token
    with exn ->
      lexbuf.lex_start_p <- beg_pos;
      raise exn
  }

  | '\'' (ident as name) { STRING name }
  | ident as name {
      try
        Hashtbl.find keywords name
      with Not_found ->
        IDENT name
  }

  | digit+ as n            { INT (float_of_string n) }
  | digit+ '.' digit+ as n { FLOAT (float_of_string n) }

  | whitespace { read lexbuf }

  | newline {
      Lexing.new_line lexbuf;
      read lexbuf
  }

  | "#" { read_comment lexbuf }

  | eof { EOF }

  | _ as ch {
      error (Printf.sprintf "Unexpected symbol: '%c'" ch) lexbuf
  }

and read_string buf =
  parse
  | '"' { STRING (Buffer.contents buf) }

  | "\\n"  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | "\\t"  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | "\\r"  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | "\\\"" { Buffer.add_char buf '"';  read_string buf lexbuf }
  | "\\\\" { Buffer.add_char buf '\\'; read_string buf lexbuf }

  | '\\' (_ as ch) {
      error (Printf.sprintf "Illegal escape sequence: '\\%c'" ch) lexbuf
  }

  | newline {
      Lexing.new_line lexbuf;
      Buffer.add_char buf '\n';
      read_string buf lexbuf
  }

  | [^ '"' '\\' '\r' '\n']+ as text {
      Buffer.add_string buf text;
      read_string buf lexbuf
  }

  | eof { error "Unterminated string literal at end of file" lexbuf }

and read_comment =
  parse
  | [^ '\r' '\n']+ { read_comment lexbuf }
  | newline {
      Lexing.new_line lexbuf;
      read lexbuf
  }
  | eof { EOF }
