open Ast

type rval_t =
  Access_lval_t of valid_type * lval_t
| Char_lit_t of string
| Int_lit_t of int
| String_lit_t of string
| Bool_lit_t of bool
| Func_call_t of valid_type * lval_t * rval_t list
| Bin_op_t of valid_type * rval_t * bop * rval_t
| Un_op_t of valid_type * uop * rval_t

and lval_t =
  Id of valid_type * string
(*| Access_arr of string * rval*)

type stmt_t =
  Assign of lval_t * rval_t
| Return of rval_t
| Return_void
| If of rval_t * block_t * block_t
| While of rval_t * block_t
| Expr of rval_t

and block_t = {
  statements_t: stmt list;
  block_num_t: int;
}

type func_def_t = {
  fname_t : string;
  params_t : param list;
  fblock_t : block;
  ret_t: valid_type;
}

type glb_var_t = lval_t * rval_t

type program_t = glb_var_t list * func_def_t list
