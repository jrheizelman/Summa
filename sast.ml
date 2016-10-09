open Ast

type valid_type = Int | Bool | Double

type rval_t =
  Int_lit_t of int
| Bool_lit_t of bool
| Double_lit_t of float
| Bin_op_t of valid_type * rval_t * bop * rval_t
| Un_op_t of valid_type * uop * rval_t
| Access_lval_t of valid_type * lval