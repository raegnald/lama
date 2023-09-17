
let debug = ref false

open Printf
open Expr
open Types

let bool_t = Mono "bool"
let int_t = Mono "int"


let tvar_counter = ref (-1)
let fresh_tvar : symbol -> type_assignment =
  fun t ->
    incr tvar_counter;
    t, Tvar !tvar_counter

let fresh_type_variable () = snd (fresh_tvar "dummy")


let rec occurs tv = function
  | Tvar tv' -> tv' = tv
  | Fun_t (a, b) -> occurs tv a || occurs tv b
  | _ -> false


let substitute_tvar tv x = function
  | Tvar tv', u when tv' = tv -> (u, x)
  | u, Tvar tv' when tv' = tv -> (u, x)
  | eq -> eq


let rec substitute_t a b = function
  | Fun_t (x, y) ->
      if !debug then printf "\t Function:\n";
      print_subst x a b; print_subst y a b;
      Fun_t (substitute_t a b x, substitute_t a b y)
  | t when a = t ->
      print_subst t a b;
      b
  | t -> t


let rec substitute equations t : type_ =
  if !debug then printf "--- %s and constraints %s---\n" (show_type t) (show_teqs equations);
  match equations with
  | [] -> t
  | (l, r) :: rest when l = r ->
      substitute rest t
  | (Fun_t (a, b), Fun_t (c, d)) :: rest ->
      if !debug then print_endline "\tSubst. two funs\n\n";
      let equations' = (a, c) :: (b, d) :: rest in
      substitute equations' t
  | (Tvar tv, x) :: rest | (x, Tvar tv) :: rest ->
      if !debug then  print_endline "\tOne side is a tvar\n\n";
      if occurs tv x then
        failwith (sprintf "Type variable %s occurs inside the type %s" (show_typevar tv) (show_type x));
      let rest' = List.map (substitute_tvar tv x) rest
      and t' = substitute_t (Tvar tv) x t in
      substitute rest' t'
  | (l, r) :: rest ->
      let t' = substitute_t l r t in
      substitute rest t'


let rec unify : tequation list -> tequation list = function
  | [] -> []
  | (t1, t2) :: rest ->
      match (t1, t2) with
        | _, _ when t1 = t2 -> unify rest
        | (Tvar _, _) | (_, Tvar _) -> (t1, t2) :: unify rest
        | (Fun_t (a1, r1), Fun_t (a2, r2)) ->
            unify ((a1, a2) :: (r1, r2) :: rest)
        | _ -> failwith (sprintf "Type mismatch between %s and %s" (show_type t1) (show_type t2))

let rec generate_type_constraints (ctx : context) = function
  | Int _ -> int_t, []

  | Sym s ->
      (try  List.assoc s ctx, [] with Not_found -> failwith (sprintf "Unbound symbol %s" s))

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
  if !debug && List.length c > 0 then show_teqs c |> printf "Constraints: {%s}\n"

let print_debug_substitutions substitutions =
  if !debug then
    let s = show_teqs substitutions in
    printf "Substitutions: {%s}\n" s
(* *** *** *** *)

let infer ctx e =
  tvar_counter := -1; (* Resets the type variables every time we infer the type of an expression *)
  let t, c = generate_type_constraints ctx e in
  print_debug_constraints e t c;
  let substitutions = unify c in
  print_debug_substitutions substitutions;
  let t' = substitute substitutions t in
  if !debug then printf "Finally: %s\n" (show_type t');
  t'
