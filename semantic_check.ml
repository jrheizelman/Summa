open Ast

(* called to get the type of an rval *)
let type_of_rval_t = function
  Access_lval_t(t, _) -> t
| Char_lit_t(_) -> Char
| Int_lit_t(_) -> Int
| String_lit_t(_) -> String
| Bool_lit_t(_) -> Bool
| Func_call_t(t, _, _) -> t
| Bin_op_t(t, _, _, _) -> t
| Un_op_t(t, _, _) -> t

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
		(* Both are chars *)
		| (Char, Char) ->
			(match op with (Add | Sub) ->
				Bin_op_t(Char, r1, op, r2)
				| _ -> binop_err t1 t2 op)
		| _ -> binop_err t1 t2 op

let unop_err (t:valid_type) (op:uop) =
	raise(Failure("Operator " ^ (string_of_unop op) ^
		" not compatible with expression of type " ^
		(string_of_valid_type t) ^ "."))

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

(* Did not check Array, construct, makeArr, Access*)
let check_expr (e:expr) env =
	match e with
	 Literal(i) -> Literal_t(i)
	 | Noexpr -> Noexpr_t
	 | Id(s) -> let (t, st, id) = check_valid_id s env in Id_t(t, st, id)
	 | Binop(e1, op, e2) ->
	 	let(ce1, ce2) = (check_expr e1 env, check_expr e2 env) in
			check_binop ce1 ce2 op
	 | Unop(op, e) ->
	 	let ce = check_expr e env in
	 		check_unop ce op
	 | Call(n, eList) ->
	 	let checkedList = check_exprList eList env  in
	 		check_func_call n checkedList env
	 | String_Lit(s) -> String_Lit_t(s)
	 | Access(n, tag) -> let (ty, st, id) = check_valid_id n env in handle_access n tag ty
	 | Char_e(c) -> Char_t(c)
	 | Assign(l, r) ->
	 	let checked_r = check_expr r env in
	 		let checked_l = check_left_value l env in
	 			check_assign checked_l checked_r
	 | Bool_Lit(b) -> Bool_Lit_t(b)
	 | Add_at(s, a) -> let (t, st, id) = check_valid_id s env in match t with
	 		Node(l) -> let t = Node(a :: l) in
	 			ignore(symbol_table_override_decl st (SymbTable_Var(st,t,id)) (fst env, id));
	 			Add_at_t(t,s, a)
	 		| Edge(l) -> let t = Edge(a :: l) in
	 			ignore(symbol_table_override_decl st (SymbTable_Var(st,t,id)) (fst env, id));
	 			Add_at_t(t,s, a)
	 		| _ -> raise(Failure("Left side of add statement must be Node or Edge type, " ^
				string_of_valid_type t ^ " given."))

let var(s) = expr check_program (p:program) env =
	let vs = fst p in
		let fs = snd p in
			let checked_vs = check_is_vdecl_list vs env in
				let checked_fs = check_function_list fs env in
					if(check_main_exists checked_fs) then (checked_vs, checked_fs)
					else raise(Failure("Function main not found."))