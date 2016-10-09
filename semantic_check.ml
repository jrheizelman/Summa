open Ast

(* SAST *)

type valid_type = Int | Bool | Double

type rval_t =
  Int_lit_t of int
| Bool_lit_t of bool
| Double_lit_t of float
| Bin_op_t of valid_type * rval_t * bop * rval_t
| Un_op_t of valid_type * uop * rval_t

type program_t = rval_t list

(* SEMANTIC CHECK *)

let string_of_valid_type = function
	Int -> "int"
| Bool -> "bool"
| Double -> "double"

let type_of_rval_t = function
  Int_lit_t(_) -> Int
| Bool_lit_t(_) -> Bool
| Double_lit_t(_) -> Double
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

let rec check_rval (r:rval) =
	match r with
	 Bin_op(r1, op, r2) ->
	 	let(ce1, ce2) = (check_rval r1, check_rval r2) in
			check_binop ce1 ce2 op
	 | Un_op(op, r) ->
	 	let cr = check_rval r in
	 		check_unop cr op
	 | Bool_lit(b) -> Bool_lit_t(b)
	 | Int_lit(i) -> Int_lit_t(i)
	 | Double_lit(d) -> Double_lit_t(d)

let check_program (p:program) =
	List.map check_rval p