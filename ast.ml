type bop = Equal | Neq | Leq | Geq | Greater | Less | And | Or
| Add | Sub | Mult | Div | Mod

type uop = Neg | Not

type valid_type = Int | Double | String | Char | Bool

type param = valid_type * string

type rval =
  Access_lval of lval
| Char_lit of string
| Int_lit of int
| String_lit of string
| Bool_lit of bool
| Func_call of lval * rval list
| Bin_op of rval * bop * rval
| Un_op of uop * rval

and lval =
  Id of string
(*| Access_arr of string * rval*)

type stmt =
  Assign of lval * rval
| Return of rval
| If of rval * block * block
| While of rval * block
| Expr of rval

and block = {
  statements: stmt list;
  block_num: int;
}

type func_def = {
  fname : string;
  params : param list;
  fblock : block;
}

type glb_var = lval * rval

type program = glb_var list * func_def list

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
| And -> "&&"
| Or -> "||"

let string_of_unop = function
  Neg -> "-"
| Not -> "!"

let string_of_valid_type = function
  Int -> "int"
| Double -> "double"
| Char -> "char"
| String -> "string"
| Bool -> "bool"

let string_of_param p = string_of_valid_type (fst p) ^ " " ^ (snd p)

let string_of_lval l = match l with
  Id(s) -> s

let rec string_of_rval = function
  Access_lval(l) -> string_of_lval l
| Char_lit(s) -> s
| Int_lit(i) -> string_of_int i
| String_lit(s) -> s
| Bool_lit(b) -> string_of_bool b
| Func_call(l, r_list) -> string_of_lval l ^ "(" ^
    (String.concat ", " (List.map string_of_rval r_list)) ^ ")"
| Bin_op(r1, b, r2) -> string_of_rval r1 ^ " " ^ string_of_bop b ^
    " " ^ string_of_rval r2
| Un_op(u, r) -> string_of_unop u ^ " " ^ string_of_rval r

let string_of_glb_var (l, r) = string_of_lval l ^ " = " ^ string_of_rval r

let rec string_of_block (b:block) = "{\n" ^
  String.concat "\n" (List.map string_of_stmt b.statements) ^
  "\n}"

  and string_of_stmt = function
    Assign(l, r) -> string_of_lval l ^ " = " ^ string_of_rval r ^ ";"
  | Return(r) -> "return " ^ string_of_rval r ^ ";"
  | If(r, b1, b2) ->
      (match b2.statements with
        [] -> "if (" ^ string_of_rval r ^ ")\n" ^ string_of_block b1
      | _  -> "if (" ^ string_of_rval r ^ ")\n" ^
              string_of_block b1 ^ "\nelse\n" ^ string_of_block b2)
  | While(r, b) ->  "while (" ^ string_of_rval r ^ ")\n" ^ string_of_block b
  | Expr(r) -> string_of_rval r ^ ";"

let string_of_func fdef = "def: " ^ fdef.fname ^ "(" ^
  String.concat ", " (List.map string_of_param fdef.params) ^ ")" ^
  string_of_block fdef.fblock

let string_of_prog prog =
  String.concat ";\n" (List.map string_of_glb_var (fst prog)) ^
  (if (List.length (fst prog)) > 0 then ";\n" else "") ^
  String.concat "\n" (List.map string_of_func (snd prog)) ^ "\n"
