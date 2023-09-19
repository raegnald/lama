
open Printf
open Expr

let debug = ref false


type type_ =
  | Mono of symbol
  | Tvar of int
  | Fun_t of type_ * type_
  | Unit_t

type context = type_assignment list
and type_assignment = symbol * type_

type tequation = type_ * type_  (* Type equation *)


let show_typevar i =
  let subindex n =
    let n' = Bytes.of_string (string_of_int n)
    and f = function '0' -> "₀" | '1' -> "₁" | '2' -> "₂" | '3' -> "₃" | '4' -> "₄" | '5' -> "₅" | '6' -> "₆" | '7' -> "₇" | '8' -> "₈" | '9' -> "₉" | c -> String.make 1 c in
    Bytes.fold_left (fun acc c -> acc ^ f c) "" n'
  in
  let greek = [| "α"; "β"; "γ"; "δ"; "ε"; "ζ"; "η"; "θ"; "ι"; "κ"; "λ"; "μ"; "ν"; "ξ"; "ο"; "π"; "ρ"; "σ"; "τ"; "υ"; "φ"; "χ"; "ψ"; "ω" |]
  and greek_len = 24 in
  if i < 0 then failwith "show_typevar: negative typevar";
  if i < greek_len
    then greek.(i)                         (* α, β, γ, ... *)
    else "τ" ^ (subindex (i - greek_len))  (* τ₁, τ₂, τ₃, ... *)

let rec show_type ?(parens=false) = function
  | Mono t -> t
  | Tvar tv -> show_typevar tv
  | Fun_t (a, b) ->
    if parens
      then sprintf "(%s → %s)" (show_type ~parens:true a) (show_type b)
      else sprintf "%s → %s" (show_type ~parens:true a) (show_type b)
  | Unit_t -> "unit"

let show_teqs (constraints : tequation list) =
  let show_constraint (t1, t2) = sprintf "\t%s = %s\n" (show_type t1) (show_type t2) in
  List.(map show_constraint constraints |> fold_left (^) "\n")

let print_subst t a b = if !debug then printf "\t(%s){%s / %s}\n" (show_type t) (show_type b) (show_type a)