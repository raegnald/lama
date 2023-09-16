
let debug = ref false

open Printf
open Expr

type type_ =
  | Mono of symbol
  | Tvar of int
  | Fun_t of type_ * type_
  | Unit_t

type context = type_assignment list
and type_assignment = symbol * type_

type constraints = tequation list
and tequation = type_ * type_  (* Type equation *)

type substitution = type_ * type_  (* from ... to ... *)


let show_typevar i =
  if i < 0 then failwith "show_typevar negative typevar";
  if i < 26 then sprintf "'%c" (char_of_int (i + 97))  (* a, b, c, d, ...*)
            else sprintf "'t%d" (i - 25)               (* t_1, t_2, t_3, ... *)

let rec show_type ?(parens=false) = function
  | Mono t -> t
  | Tvar tv -> show_typevar tv
  | Fun_t (a, b) ->
    if parens
      then sprintf "(%s -> %s)" (show_type ~parens:true a) (show_type b)
      else sprintf "%s -> %s" (show_type ~parens:true a) (show_type b)
  | Unit_t -> "unit"

let show_constraints (constraints : constraints) =
  let show_constraint (t1, t2) = sprintf "\t%s = %s\n" (show_type t1) (show_type t2) in
  List.(map show_constraint constraints |> fold_left (^) "\n")
let show_substitutions : substitution list -> string = show_constraints


let bool_t = Mono "bool"
let int_t = Mono "int"


let tvar_counter = ref (-1)
let fresh_tvar : symbol -> type_assignment =
  fun t ->
    incr tvar_counter;
    t, Tvar !tvar_counter

let fresh_type_variable () = snd (fresh_tvar "dummy")


let[@warning "-8"] rec occurs (Tvar tv) = function
  | Tvar tv' -> tv' = tv
  | Fun_t (a, b) -> a = (Tvar tv) || occurs (Tvar tv) b
  | _ -> false

let print_subst t a b = printf "\t(%s){%s / %s}\n" (show_type t) (show_type b) (show_type a)


let rec apply_substitution_type (sub : substitution list) (t : type_) : type_ =
  match t with
  | Mono x -> Mono x
  | Tvar i -> apply_substitution_type_variable sub i
  | Fun_t (t1, t2) ->
    let t1' = apply_substitution_type sub t1 in
    let t2' = apply_substitution_type sub t2 in
    Fun_t (t1', t2')
  | Unit_t -> Unit_t

and apply_substitution_type_variable (sub : substitution list) (i : int) : type_ =
  match find_substitution sub i with
  | None -> Tvar i
  | Some (_, t) -> apply_substitution_type sub t

and find_substitution (sub : substitution list) (i : int) : substitution option =
  match sub with
  | [] -> None
  | (t1, t2) :: rest ->
    if t1 = Tvar i then Some (t1, t2)
    else find_substitution rest i

let apply_substitution_context (sub : substitution list) (ctx : context) : context =
  List.map (fun (x, t) -> (x, apply_substitution_type sub t)) ctx

let apply_substitution_constraints (sub : substitution list) (eqs : constraints) : constraints =
  List.map (fun (t1, t2) -> (apply_substitution_type sub t1, apply_substitution_type sub t2)) eqs


let rec unify (eqs : constraints) : substitution list =
  match eqs with
  | [] -> []
  | (t1, t2) :: rest ->
    if t1 = t2 then unify rest
    else
      match (t1, t2) with
      | (Tvar _, _) | (_, Tvar _) -> (t1, t2) :: unify rest
      | (Fun_t (a1, r1), Fun_t (a2, r2)) ->
        unify ((a1, a2) :: (r1, r2) :: rest)
      | _ -> failwith "Type mismatch"

let rec generate_type_constraints (ctx : context) = function
  | Int _ -> int_t, []

  | Sym s -> List.assoc s ctx, []
  
  | Fun (x, e') ->
      let t1 = fresh_tvar x in
      let ctx' = t1::ctx in
      let t2, body_constraints = generate_type_constraints ctx' e' in
      Fun_t (snd t1, t2), body_constraints

  | App (e1, e2) ->
      let t = fresh_type_variable () in
      let t1, c1 = generate_type_constraints ctx e1
      and t2, c2 = generate_type_constraints ctx e2 in
      let c = [ t1, Fun_t (t2, t) ] in
      t, List.concat [c2; c1; c]

  | Let (x, e1, e2) ->
      let t1, c1 = generate_type_constraints ctx e1 in
      let ctx' = (x, t1)::ctx in
      let t2, c2 = generate_type_constraints ctx' e2 in
      t2, List.concat [c2; c1]

  | Unit -> Unit_t, []


(* *** Debugging functions *** *)
let print_debug_constraints e t c =
  if !debug then printf "\t%s : %s\n"  (show_expr e) (show_type t);
  if !debug && List.length c > 0 then show_constraints c |> printf "Constraints: {%s}\n"

let print_debug_substitutions substitutions =
  if !debug then
    let s = show_substitutions substitutions in
    printf "Substitutions: {%s}\n" s
(* *** *** *** *)

let infer ctx e =
  tvar_counter := -1; (* Reset the type variables *)
  let t, c = generate_type_constraints ctx e in
  print_debug_constraints e t c;
  let substitutions = unify c in
  print_debug_substitutions substitutions;
  apply_substitution_type substitutions t
