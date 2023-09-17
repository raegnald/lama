
open Printf
open Lama

let _debug = ref begin
  let d = try Array.exists (String.equal "--debug") Sys.argv
          with Not_found -> false
  in
  Infer.debug := d;
  Types.debug := d;
  d
end

let parse src =
  let lexbuf = Lexing.from_string src in
  Parser.prog Lexer.read lexbuf

let rec toplevel ?(prompt="→") f =
  printf "%s " prompt;
  begin try
    f (read_line ())
  with
    | Failure msg -> printf "× %s\n" msg
    | Not_found -> print_endline "× Not found"
    | Parser.Error -> print_endline "× Parsing error"
  end;
  toplevel ~prompt f

let ctx =
  let open Types in
  [
    "add"  , Fun_t (Mono "int", Fun_t (Mono "int", Mono "int"))
  ; "succ" , Fun_t (Mono "int", Mono "int")
  ; "id"   , Fun_t (Tvar 0, Tvar 0)
  ; "true" , Mono "bool"
  ; "false", Mono "bool"
  ; "if"   , Fun_t (Mono "bool", Fun_t (Tvar 0, Fun_t (Tvar 0, Tvar 0)))
  ; "const", Fun_t (Tvar 0, Fun_t (Tvar 1, Tvar 0))
  ]

let () = toplevel begin
  fun src ->
    let expr = parse src in
    let t = Infer.infer ctx expr in
    printf "⊢ %s : %s\n" (Expr.show_expr expr) (Types.show_type t)
  end
