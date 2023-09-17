
type symbol = string

type expr =
  | Sym of symbol
  | Fun of symbol * expr
  | App of expr   * expr
  | Let of symbol * expr * expr
  | Unit
  | Int of int

let rec show_expr ?(parens=false) =
  let open Printf in function
  | Sym s -> s
  | Fun (param, e) ->
      let e' = (* Not rendering a space between function parameters: "[x][y] ..." instead of "[x] [y] ..." *)
        let e'' = show_expr e in
        match e with
          | Fun _ -> e''
          | _ -> " " ^ e''
      in
      if parens
        then sprintf "([%s]%s)" param e'
        else sprintf "[%s]%s" param e'
  | App (e1, e2) ->
      if parens
        then sprintf "(%s %s)" (show_expr e1) (show_expr ~parens:true e2)
        else sprintf "%s %s" (show_expr e1) (show_expr ~parens:true e2)
  | Let (x, e1, e2) ->
      sprintf "let %s = %s in %s" x (show_expr e1) (show_expr e2)
  | Unit -> "()"
  | Int n -> string_of_int n
