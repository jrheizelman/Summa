type bop = Equal | Neq | Leq | Geq | Greater | Less | And | Or
| Add | Sub | Mult | Div | Mod

type uop = Neg | Not

type lval =
  Id of string
| Access_arr of lval * rval

and rval =
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

type valid_type = Int | Bool | Double | Array of arr_type

and arr_type = valid_type * int

let rec id_of_lval = function
  Id(id) -> id
| Access_arr(l, _) -> id_of_lval l

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

let rec string_of_valid_type = function
  Int -> "int"
| Bool -> "bool"
| Double -> "double"
| Array(t,d) -> "array, t " ^ string_of_valid_type t ^ ", d " ^ string_of_int d

let string_of_unop = function
  Neg -> "-"
| Not -> "!"

let rec string_of_lval = function
  Id(i) -> "id " ^ i
| Access_arr(l, r) -> "arr_access " ^ string_of_lval l ^ "[" ^ string_of_rval r ^ "]"

and string_of_rval = function
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