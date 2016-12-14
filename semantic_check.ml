open Ast
open Sast
open Symbol_table

let match_err (v1:valid_type) (v2:valid_type) =
  raise(Failure("Types " ^ string_of_valid_type v1 ^ " and " ^
    string_of_valid_type v2 ^ " are not compatible."))

(* Checks compatibility between monotype and polytype, returns env
  If the polytype is a reference resolved to monotype, will overwrite poly *)
let rec mono_is_poly mono gl poly id env = match poly with
  Reference(s) -> let vt = symbol_table_get_id env s in (match vt with
    Mono(m, _) ->
      if mono == m then symbol_table_replace_id env id (Mono(mono, gl))
      else match_err (Mono(m, gl)) (Poly(poly))
  | Poly(p2) -> mono_is_poly mono gl p2 s env)
| Conditioned(c_list) -> (match c_list with
    hd :: tl ->
      if not (hd (Mono(mono, gl))) then
        match_err (Mono(mono, gl)) (Poly(poly))
      else (mono_is_poly mono gl (Conditioned(tl)) id env)
  | [] -> symbol_table_replace_id env id (Mono(mono, gl)))
| Function(_, _) -> match_err (Mono(mono, gl)) (Poly(poly))
| Grouping(g) -> (match gl with
    hd :: tl -> if g == hd then env else mono_is_poly mono tl poly id env
  | [] -> match_err (Mono(mono, gl)) (Poly(poly)))

(* Checks compatibility from polytypes r1 to r2, on success:
  - Returns match with most restrictive valid_type * env
  - Overwrites r1, r2, and any referenced types in env *)
let rec poly_is_poly poly1 id1 poly2 id2 env = match poly1 with
  Reference(ref_id) ->
    (let ref_vt = symbol_table_get_id env ref_id in match ref_vt with
      Mono(ref_m, ref_g) -> let env = mono_is_poly ref_m ref_g poly2 id2 env in
        symbol_table_replace_id env id1 (Mono(ref_m, ref_g))
    | Poly(ref_p) -> let env = poly_is_poly ref_p ref_id poly2 id2 env in
        let ret_vt = symbol_table_get_id env ref_id in
          symbol_table_replace_id env id1 ret_vt)
| Conditioned(c_list1) -> (match poly2 with
    Conditioned(c_list2) -> ()
  | _ -> poly_is_poly poly2 id2 poly1 id1 env)
| Function(pt_list1, rt1) -> ()
| Grouping(g) -> ()

(* Error raised for improper binary operation *)
let binop_err (t1:valid_type) (t2:valid_type) (op:bop) =
  raise(Failure("Operator " ^ (string_of_bop op) ^
    " not compatible with expressions of type " ^
    string_of_valid_type t1 ^ " and  " ^
    string_of_valid_type t2 ^ "."))

(* Check binary operation, returns rval_t * env on success *)
let rec check_binop (r1:rval_t) (r2:rval_t) (op:bop) env =
  let (t1, t2) = (type_of_rval_t r1, type_of_rval_t r2) in
    match t1 with
      Mono(m1, gl1) -> (match t2 with
        (* Both r1 and r2 are monotypes *)
        Mono(m2, gl2) -> (match op with
          (Equal | Neq | Leq | Geq | Greater | Less | Add) ->
            (match (m1,m2) with
              ((String, String) | (Int, Int) | (Double, Double) | (Char, Char))
                -> (Bin_op_t(t1, r1, op, r2), env)
            | (Bool, Bool) -> if op == Equal then
                (Bin_op_t(t1, r1, op, r2), env)
              else binop_err t1 t2 op
            | _ -> binop_err t1 t2 op)
        | (Sub | Mult | Div | Mod) ->
            (match (m1, m2) with
              ((Int, Int) | (Double, Double)) -> (Bin_op_t(t1, r1, op, r2), env)
            | _ -> binop_err t1 t2 op)
        | _ -> binop_err t1 t2 op)
        (* r1 is a monotype, r2 is polytype *)
      | Poly(p2) -> (
          let (check, env) = mono_is_poly m1 gl1 p2 env in
            if check then
              let (r2, env) = write_rval_t t1 r2 env in check_binop r1 r2 op env
            else binop_err t1 t2 op))
    | Poly(p1) -> (match t2 with
        Mono(m2, gl2) -> let (check, env) = mono_is_poly m2 gl2 p1 env in
          if check then
            let (r1, env) = write_rval_t t2 r1 env in check_binop r1 r2 op env
      | Poly(p2) -> ()
    )

let unop_err (t:valid_type) (op:unop) =
  raise(Failure("Operator " ^ (string_of_unop op) ^
    " not compatible with expression of type " ^
    (string_of_valid_type t) ^ "."))

(* Checks unary operation for proper types *)
let check_unop (r:rval_t) (op:unop) env = env

(* Checks rval access for any errors, returns (rval_t, env) *)
let rec check_rval (r:rval) env =
  match r with
    Bin_op(r1, op, r2) ->
      let(ce1, ce2) = (check_rval r1 env, check_rval r2 env) in
        check_binop ce1 ce2 op
  | Un_op(op, r) ->
      let cr = check_rval r env in
        check_unop cr op
  | Bool_lit(b) -> Bool_lit_t(b)
  | Int_lit(i) -> Int_lit_t(i)
  | Double_lit(d) -> Double_lit_t(d)
  | Access_lval(l) -> Access_lval_t(check_lval l env, l)
  | Noexpr -> Noexpr_t(Void
you)  | Increment(i, l) ->
      let t = check_lval l env in
      if equals t Int then Increment_t(i, l)
      else raise(Failure("Increment only valid on type int, " ^ string_of_valid_type t ^ " received."))

(* Checks lval access for any errors, returns validtype *)
and check_lval (l:lval) env =
  match l with
    Id(id) -> symbol_table_get_id (id_of_lval l) env
  | Access_arr(l, r) ->
      if not (type_of_rval_t (check_rval r env) = Int) then
        raise(Failure("Value inside array brackets must be integer."))
      else let l_type = check_lval l env in
        match l_type with
          Array(t, d) -> if d == 1 then t else Array(t, d-1)
        | _ -> raise(Failure("Trying to access non-array " ^ (id_of_lval l) ^ " as array."))

let get_reference_id (r:rval) env =
  match (check_rval r env) with
    Access_lval_t(t, l) -> (id_of_lval l)
  | _ -> raise(Failure("get_reference_id call made on non-lval type."))

(* Checks assignment, adds to table when new, checks type if existing. Returns s_table *)
let check_assign (l:lval) (r:rval) env =
  let new_t = type_of_rval_t (check_rval r env) in
    (match new_t with
      Undef -> ignore (new_t = Ref_to_type(get_reference_id r env, snd env));
    | Array(vt, d) -> (match vt with Undef -> ignore (new_t = Ref_to_type(get_reference_id r env, snd env)); | _ -> ();)
    | _ -> (););
    try(
      let prev_t = check_lval l env in
        if not (equals prev_t new_t) then
          (* Add new entry at lower scope with new type *)
          symbol_table_add_id env (id_of_lval l) new_t
        else env)
    with Failure(f) -> (* id of lval is not present in the table *)
      match l with
        Id(id) -> symbol_table_add_id env id (type_of_rval_t (check_rval r env))
      | Access_arr(l, _) -> raise(Failure("Array " ^ (id_of_lval l) ^ " not found."))

(* Checks statements for semantic errors, returns env *)
let rec check_stmt env (s:stmt) =
  match s with
    Assign(l, r) -> check_assign l r env
  | Rval(r) -> ignore (check_rval r env); env
  | Return(r) -> ignore (check_rval r env); env
  | If(r, b1, b2) ->
      let r_type = type_of_rval_t (check_rval r env) in
        if equals r_type Bool then (
          ignore (check_block b1 env); ignore (check_block b2 env); env)
        else raise(Failure("Value inside if statement condition must be bool, received: " ^
                           string_of_valid_type r_type ^ "."))
  | While(r, b) ->
      let r_type = type_of_rval_t (check_rval r env) in
        if equals r_type Bool then (
          ignore (check_block b env); env)
        else raise(Failure("Value inside while statement condition must be bool, received: " ^
                           string_of_valid_type r_type ^ "."))
  | For(s1, r, s2, b) ->
      let r_type = type_of_rval_t (check_rval r env) in
        if equals r_type Bool then let env_for = check_stmt env s1 in
          let env_for = check_stmt env_for s2 in
            ignore (check_block b env_for); env
        else raise(Failure("Value inside for statement condition must be bool, received: " ^
                           string_of_valid_type r_type ^ "."))

and check_block (b:block) env =
  let (table, _) = env in
    List.fold_left check_stmt (table, b.block_num) b.stmts

(* Adds the function def with type to table, then checks block, returns env *)
let check_func_helper (f:func_def) (f_type:valid_type) env =
  let env = symbol_table_add_id env f.id f_type in
    let add_params = (fun env (t, id) -> symbol_table_add_id env id t) in
      let env = List.fold_left add_params env f.params in
        check_block f.body_block env

let check_func_def (f:func_def) env =
  let f_type = Function(f.ret_type, (List.map fst f.params)) in
    try(
      let prev_t = symbol_table_get_id f.id env in
        if not (equals prev_t f_type) then
          (print_endline ("Warning: Id " ^ f.id ^
                         " was already assigned type " ^ string_of_valid_type prev_t);
          ignore (symbol_table_replace_id env f.id f_type);
          check_func_helper f f_type env)
        else env)
    with Failure(fail) ->
      check_func_helper f f_type env

(* Checks program for semantic errors, returns env *)
let check_program (p:program) =
  check_func_def p ((Hashtbl.create 1000), 0)
