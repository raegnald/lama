%{
  open Expr
%}

%token<string> SYMBOL  // x y' __zA_'''?
%token<int> INT
%token LBRACK RBRACK   // [ ]
%token LPAREN RPAREN   // ( )
%token LET EQ IN

%token EOF

%start <expr> prog

%nonassoc SYMBOL INT LBRACK LPAREN LET
%left APP // Solving precedence of function applications

%%

prog:
  | e = expr; EOF
      { e }

expr:
  | param = bracketed(SYMBOL); body = expr
      { Fun (param, body) }

  (* Syntax sugar for lambdas: [x y z] ε ===> [x][y][z] ε *)
  | params = bracketed(SYMBOL+); body = expr
      { List.fold_left (fun acc param -> Fun (param, acc)) body (List.rev params) }

  (* Function application *)
  | f = expr; arg = expr %prec APP
      { App (f, arg) }

  | LET; s = SYMBOL; EQ; e1 = expr; IN; e2 = expr
      { Let (s, e1, e2) }

  | LPAREN; RPAREN
      { Unit }

  | s = SYMBOL
      { Sym s }

  | n = INT
      { Int n }

  | e = parens(expr)
    { e }

parens(e):
  | LPAREN; e = e; RPAREN
      { e }

bracketed(e):
  | LBRACK; e = e; RBRACK
      { e }
