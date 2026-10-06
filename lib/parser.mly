%{
  open Ast
%}

%token DOT
%token DOTDOT
%token COMMA
%token COLON
%token SEMICOLON
%token LPAREN
%token RPAREN
%token LCURLY
%token RCURLY
%token LBRACK
%token RBRACK
%token ADD
%token SUB
%token MUL
%token DIV
%token REM
%token ASSIGN
%token NOT
%token LSS
%token GTR
%token BAR
%token EQL
%token NEQ
%token LEQ
%token GEQ
%token ARROW
%token PIPE
%token PERCENT_LCURLY
%token EOF

%token AND_KW
%token FALSE_KW
%token FN_KW
%token MATCH_KW
%token NIL_KW
%token OR_KW
%token TRUE_KW
%token WHEN_KW

%token <string> IDENT
%token <float>  INT
%token <float>  FLOAT
%token <string> STRING

%token NEG /* pseudo token for unary minus */

%right ASSIGN
%left PIPE
%left OR_KW
%left AND_KW
%left EQL NEQ
%left LSS GTR LEQ GEQ
%left ADD SUB
%left MUL DIV REM
%right NOT NEG

%start <Ast.expr> prog

%%

prog:
  | lst = expr_list; EOF
      {
        let exprs, tail = lst in
        Prog (exprs, tail)
      }
  ;

block:
  | LCURLY; lst = expr_list; RCURLY
      {
        let exprs, tail = lst in
        Block (exprs, tail)
      }
  ;

expr_list:
  | /**/
      { ([], None) }
  | e = expr
      { ([], Some e) }
  | e = expr; SEMICOLON; rest = expr_list
      {
        let (exprs, tail) = rest in
        (e :: exprs, tail)
      }

expr:
  | lhs = expr; ADD; rhs = expr
      { Binary (Add, lhs, rhs) }
  | lhs = expr; SUB; rhs = expr
      { Binary (Sub, lhs, rhs) }
  | lhs = expr; MUL; rhs = expr
      { Binary (Mul, lhs, rhs) }
  | lhs = expr; DIV; rhs = expr
      { Binary (Div, lhs, rhs) }
  | lhs = expr; REM; rhs = expr
      { Binary (Rem, lhs, rhs) }
  | lhs = expr; ASSIGN; rhs = expr
      { Binary (Assign, lhs, rhs) }
  | lhs = expr; LSS; rhs = expr
      { Binary (Lss, lhs, rhs) }
  | lhs = expr; GTR; rhs = expr
      { Binary (Gtr, lhs, rhs) }
  | lhs = expr; EQL; rhs = expr
      { Binary (Eql, lhs, rhs) }
  | lhs = expr; NEQ; rhs = expr
      { Binary (Neq, lhs, rhs) }
  | lhs = expr; LEQ; rhs = expr
      { Binary (Leq, lhs, rhs) }
  | lhs = expr; GEQ; rhs = expr
      { Binary (Geq, lhs, rhs) }
  | lhs = expr; AND_KW; rhs = expr
      { Binary (And, lhs, rhs) }
  | lhs = expr; OR_KW; rhs = expr
      { Binary (Or, lhs, rhs) }
  | lhs = expr; PIPE; rhs = expr
      { Binary (Pipe, lhs, rhs) }
  | NOT; e = expr
      { Unary (Not, e) }
  | SUB; e = expr %prec NEG
      { Unary (Neg, e)}
  | e = postfix_expr
      { e }
  ;

postfix_expr:
  | func = postfix_expr; LPAREN; exprs = separated_list(COMMA, expr); RPAREN
      { Call (func, exprs) }
  | target = postfix_expr; DOT; key = IDENT
      { Index (target, String key) }
  | target = postfix_expr; LBRACK; index = expr; RBRACK
      { Index (target, index) }
  | e = primary_expr
      { e }
  ;

primary_expr:
  | i = IDENT
      { Ident i }
  | n = INT
      { Number n }
  | n = FLOAT
      { Number n }
  | s = STRING
      { String s }
  | TRUE_KW
      { Boolean true }
  | FALSE_KW
      { Boolean false }
  | NIL_KW
      { Nil }
  | e = block
      { e }
  | e = match_expr
      { e }
  | e = func_expr
      { e }
  | e = list_expr
      { e }
  | e = map_expr
      { e }
  | LPAREN; e = expr; RPAREN
      { e }
  ;

func_expr:
  | FN_KW; LPAREN; params = separated_list(COMMA, IDENT); RPAREN; fnbody = block
      { Func (params, fnbody) }
  ;

match_expr:
  | MATCH_KW; subject = expr; LCURLY; arms = match_arms; RCURLY
      { Match (subject, arms) }
  ;

match_arms:
  | arm = match_arm
      { [arm] }
  | arm = match_arm; COMMA
      { [arm] }
  | arm = match_arm; COMMA; rest = match_arms
      { arm :: rest }
  ;

match_arm:
  | pat = pattern; guard = option(guard); ARROW; res = expr
      { { pat; guard; res } }
  ;

guard:
  | WHEN_KW; e = expr
      { e }
  ;

pattern:
  | p = or_pattern
      { p }
  ;

or_pattern:
  | p = primary_pattern
      { p }
  | p1 = or_pattern; BAR; p2 = primary_pattern
      { PatOr (p1, p2) }
  ;

primary_pattern:
  | i = IDENT
      { if i = "_" then PatWildcard else PatVar i }
  | p = literal_pattern
      { PatLit p }
  | p = list_pattern
      { p }
  | p = map_pattern
      { p }
  ;

literal_pattern:
  | n = INT
    { Number n }
  | SUB; n = INT
      { Unary (Neg, Number n) }
  | s = STRING
      { String s }
  | TRUE_KW
      { Boolean true }
  | FALSE_KW
      { Boolean false }
  | NIL_KW
      { Nil }
  ;

list_expr:
  | LBRACK; RBRACK;
      { List ([], None) }
  | LBRACK; DOTDOT; rest = expr; RBRACK
      { List ([], Some rest) }
  | LBRACK; elems = list_elems ; RBRACK
      {
        let (exprs, rest) = elems in
        List (exprs, rest)
      }
  ;

list_elems:
  | e = expr
      { ([e], None) }
  | e = expr; COMMA; DOTDOT; rest = expr
      { ([e], Some rest) }
  | e = expr; COMMA; rest = list_elems
      {
        let (exprs, tail) = rest in
        (e :: exprs, tail)
      }
  ;

map_expr:
  | PERCENT_LCURLY; entries = separated_list(COMMA, map_entry); RCURLY
      { Map entries }
  ;

map_entry:
  | key = expr; COLON; value = expr
      { (key, value) }
  ;

list_pattern:
  | LBRACK; RBRACK
      { PatList [] }
  | LBRACK; rest = rest_pattern; RBRACK
      { PatListRest ([], rest) }
  | LBRACK; pats = list_elem_patterns; RBRACK
      { pats }
  ;

rest_pattern:
  | DOTDOT; rest = option(IDENT)
      { rest }
  ;

list_elem_patterns:
  | p = pattern
      { PatList [p] }
  | p = pattern; COMMA; rest = rest_pattern
      { PatListRest ([p], rest) }
  | p = pattern; COMMA; rest = list_elem_patterns
      {
        match rest with
        | PatList ps -> PatList (p :: ps)
        | PatListRest (ps, rest) -> PatListRest (p :: ps, rest)
        | other -> PatList [p; other]
      }
  ;

map_pattern:
  | PERCENT_LCURLY; entries = separated_list(COMMA, map_entry_pattern); RCURLY
      { PatMap entries }
  ;

map_entry_pattern:
  | key = expr; COLON; pat = pattern
      { (key, pat) }
  ;