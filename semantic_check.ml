open Ast
open Sast
open Symbol_table

(* Error raised for improper binary operation *)
let binop_err (t1:valid_type) (t2:valid_type) (op:bop) =
    raise(Failure("Operator " ^ (string_of_bop op) ^
      " not compatible with expressions of type " ^
      string_of_valid_type t1 ^ " and  " ^
      string_of_valid_type t2 ^ "."))

(* Check binary operation *)
let check_binop (r1:rval_t) (r2:rval_t) (op:bop) =
  let (t1, t2) = (type_of_rval_t r1, type_of_rval_t r2) in
    (* Both are ints *)
    match (t1, t2) with (Int, Int) ->
      (match op with
        (Add | Sub | Mult | Div | Mod) -> Bin_op_t(Int, r1, op, r2)
        | (Equal | Neq | Less | Leq | Greater | Geq) -> Bin_op_t(Bool, r1, op, r2)
        | _ -> binop_err t1 t2 op)
    (* Both are bools *)
    | (Bool, Bool) ->
      (match op with (And | Or | Equal | Neq) ->
        Bin_op_t(Bool, r1, op, r2)
        | _ -> binop_err t1 t2 op)
    | (Double, Double) ->
      (match op with
        (Add | Sub | Mult | Div | Mod) -> Bin_op_t(Double, r1, op, r2)
        | (Equal | Neq | Less | Leq | Greater | Geq) -> Bin_op_t(Bool, r1, op, r2)
        | _ -> binop_err t1 t2 op)
    | _ -> binop_err t1 t2 op

let unop_err (t:valid_type) (op:uop) =
  raise(Failure("Operator " ^ (string_of_unop op) ^
    " not compatible with expression of type " ^
    (string_of_valid_type t) ^ "."))

(* Checks unary operation for proper types *)
let check_unop (r:rval_t) (op:uop) =
  let t = type_of_rval_t r in
    match t with
    (* Expression is an int *)
       Int ->
        (match op with
          Neg -> Un_op_t(Int, op, r)
        | _ -> unop_err t op)
    (* Expression is a bool *)
    | Bool ->
      (match op with
          Not -> Un_op_t(Bool, op, r)
          | _ -> unop_err t op)
    | _ -> unop_err t op

(* Checks rval access for any errors, returns rval_t *)
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
  | Noexpr -> Noexpr_t(Void)
  | Decl(t) -> Decl_t(t)
  | Increment(i, l) ->
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

(* Checks assignment, adds to table when new, checks type if existing. Returns env *)
let check_assign (l:lval) (r:rval) env =
  try(
    let prev_t = check_lval l env in
      let new_t = type_of_rval_t (check_rval r env) in
        if not (equals prev_t new_t) then
          (print_endline ("Warning: Id " ^ (id_of_lval l) ^
                         " was already assigned type " ^ string_of_valid_type prev_t);
          symbol_table_replace_id (id_of_lval l) new_t env)
        else env)
  with Failure(f) -> (* id of lval is not present in the table *)
    match l with
      Id(id) -> symbol_table_add_id id (type_of_rval_t (check_rval r env)) env
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

(* Checks program for semantic errors, returns env *)
let check_program (p:program) =
  check_block p ((Hashtbl.create 1000), 0)