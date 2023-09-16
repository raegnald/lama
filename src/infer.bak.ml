
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
let fresh_t : symbol -> type_assignment =
  fun t ->
    incr tvar_counter;
    t, Tvar !tvar_counter

let fresh_tvar () = snd (fresh_t "dummy")


let[@warning "-8"] rec occurs (Tvar tv) = function
  | Tvar tv' -> tv' = tv
  | Fun_t (a, b) -> a = (Tvar tv) || occurs (Tvar tv) b
  | _ -> false

let print_subst t a b = printf "\t(%s){%s / %s}\n" (show_type t) (show_type b) (show_type a)

(** [substitute a b t] is t{b/a}. This is, it substitues a to b inside
    the type t *)
let rec substitute_t a b t = match t with
  | Fun_t (x, y) ->
      printf "\t Function:\n";
      print_subst x a b; print_subst y a b;
      Fun_t (substitute_t a b x, substitute_t a b y)
  | t when a = t ->
      print_subst t a b;
      b
  | _ -> t

let rec substitute substitutions t =
  printf "Substitutions = {%s}\n" (show_substitutions substitutions);
  match substitutions with
  | [] -> t
  | (a, b)::rest ->
      let t' = (substitute_t a b t) in
      substitute rest t'

let rec unify expr : tequation -> substitution list = function
  (* Trivial unifications *)
  | t, t' when t = t' -> []

  | Tvar tv, t | t, Tvar tv -> [t, Tvar tv]

  (* unify (a -> b = c -> d) ==> unify (a = c) && unify (b = d)  *)
  | Fun_t (a, b), Fun_t (c, d) ->
      List.concat [unify expr (a, c); unify expr (b, d)]

  | t1, t2 -> failwith (sprintf "Unification unfeasible for types %s and %s in expression\n\t%s" (show_type t1) (show_type t2) (Expr.show_expr expr))


let rec generate_type_constraints (ctx : context) expr : type_ * constraints =
  (* if !debug then begin
    printf "\t%s\nΓ =" (Expr.show_expr expr);
    List.iter (fun assignment -> printf "\t · %s : %s\n" (fst assignment) (show_type (snd assignment))) ctx;
    print_endline "";
  end; *)
  match expr with
  (* {} ⊢ n : int ⊣ {} <=> n ∈ ℤ *)
  | Int _ -> int_t, []

  (* Γ ⊢ s : Γ(s) ⊣ {} *)
  | Sym s -> List.assoc s ctx, []

  (* Γ ⊢ [x] ε : τ₁ t₂ => τ₁ -> t₂ ⊣ C *)
  | Fun (param, body) ->
      let t1 = fresh_t param in
      let ctx' = t1::ctx in
      let t2, body_constraints = generate_type_constraints ctx' body in
      Fun_t (snd t1, t2), body_constraints

  (* Γ ⊢ ε₁ ε₂ : τ ⊣ {C₁, C₂, C} *)
  | App (e1, e2) ->
      let t = fresh_tvar () in
      let t1, c1 = generate_type_constraints ctx e1
      and t2, c2 = generate_type_constraints ctx e2 in
      let c = [ t1, Fun_t (t2, t) ] in
      t, List.concat [c2; c1; c]

  (* {} ⊢ () : unit ⊣ {} *)
  | Unit -> Unit_t, []

  | _ -> failwith (sprintf "Cannot infer type of\n\t%s" (Expr.show_expr expr))


let infer ctx e =
  tvar_counter := -1;
  let t, c = generate_type_constraints ctx e in

  if !debug then printf "\t%s : %s\n"  (show_expr e) (show_type t);

  if !debug && List.length c > 0 then show_constraints c |> printf "Constraints: {%s}\n";

  let substitutions = List.(map (unify e) c |> filter (fun l -> List.length l <> 0)) in

  if !debug then List.iter (fun s -> show_substitutions s |> printf "Substitutions for %s:%s" (show_expr e)) substitutions;

  List.fold_left (Fun.flip substitute) t substitutions
