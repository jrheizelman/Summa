open Ast

type rval_t =
  Int_lit_t of int
| Bool_lit_t of bool
| Double_lit_t of float
| Bin_op_t of valid_type * rval_t * bop * rval_t
| Un_op_t of valid_type * unop * rval_t
| Access_lval_t of valid_type * lval
| Noexpr_t of valid_type
| Increment_t of increment * lval

let rec id_of_rval_t = function
	Un_op_t(_, _, r) -> id_of_rval_t r
| Access_lval_t(_, l) -> id_of_lval l
| Increment_t(_, l) -> id_of_lval l
| _ -> ""

let type_of_rval_t = function
  Int_lit_t(_) -> Mono(Int, [Num])
| Bool_lit_t(_) -> Mono(Bool, [])
| Double_lit_t(_) -> Mono(Double, [Num])
| Bin_op_t(t, _, _, _) -> t
| Un_op_t(t, _, _) -> t
| Access_lval_t(t, _) -> t
| Noexpr_t(t) -> t
| Increment_t(_, _) -> Mono(Int, [Num])