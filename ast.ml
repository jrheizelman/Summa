type bop = Equal | Neq | Leq | Geq | Greater | Less | And | Or
| Add | Sub | Mult | Div | Mod

type uop = Neg | Not

type lval =
  Id of string

type rval =
  Int_lit of int
| Bool_lit of bool
| Double_lit of float
| Bin_op of rval * bop * rval
| Un_op of uop * rval
| Access_lval of lval

type stmt =
  Assign of lval * rval
| Rval of rval

type program = stmt list

(*************************
**** PRINT AST **********
*************************)

let string_of_bop = function
  Add -> "+"
| Sub -> "-"
| Mult -> "*"
| Div -> "/"
| Mod -> "mod"
| Equal -> "=="
| Neq -> "!="
| Less -> "<"
| Leq -> "<="
| Greater -> ">"
| Geq -> ">="
| And -> "and"
| Or -> "or"

let string_of_unop = function
  Neg -> "-"
| Not -> "!"

let string_of_lval = function
  Id(i) -> "id " ^ i

let rec string_of_rval = function
  Int_lit(i) -> "int_lit " ^ string_of_int i
| Bool_lit(b) -> "bool_lit " ^ string_of_bool b
| Double_lit(d) -> "double_lit " ^ string_of_float d
| Bin_op(r1, b, r2) -> "bin_op { " ^ string_of_rval r1 ^ " " ^ string_of_bop b ^
    " " ^ string_of_rval r2 ^ " }"
| Un_op(u, r) -> "un_op { " ^ string_of_unop u ^ " " ^ string_of_rval r ^ " }"
| Access_lval(l) -> "access " ^ string_of_lval l

let string_of_stmt = function
  Assign(l, r) -> "assign { " ^ string_of_lval l ^ " = " ^ string_of_rval r ^ " };"
| Rval(r) -> "rval { " ^ string_of_rval r ^ " };"

let string_of_prog prog =
  String.concat ";\n" (List.map string_of_stmt prog) ^ ";"