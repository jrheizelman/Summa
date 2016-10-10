type bop = Equal | Neq | Leq | Geq | Greater | Less | And | Or
| Add | Sub | Mult | Div | Mod

type increment = Incr_front | Incr_back | Decr_front | Decr_back

type uop = Neg | Not | Increment of increment

type valid_type = Int | Bool | Double | Array of valid_type * int | Void
| Function of valid_type * valid_type list | Undef

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
| Noexpr
| Decl of valid_type
| Increment of increment * lval

type stmt =
  Assign of lval * rval
| Rval of rval
| Return of rval
| If of rval * block * block
| While of rval * block
| For of stmt * rval * stmt * block

and block = {
  stmts : stmt list;
  block_num : int;
}

type func_def = {
  id : string;
  ret_type : valid_type;
  params : (valid_type * string) list;
  body_block : block;
}

type program = func_def

let rec equals t1 t2 = match t1 with
  Int -> (match t2 with Int -> true | _ -> false)
| Bool -> (match t2 with Bool -> true | _ -> false)
| Double -> (match t2 with Double -> true | _ -> false)
| Array(at1, d1) -> (match t2 with Array(at2, d2) ->
    if d1 == d2 then equals at1 at2 else false | _ -> false)
| Void -> (match t2 with Void -> true | _ -> false)
| Function (rt1, p_list1) -> (
    match t2 with
      Function(rt2, p_list2) ->
        if (not (equals rt1 rt2) || (List.length p_list1 != List.length p_list2)) then false
        else let rec comp_lists p_list1 p_list2 =
          if List.length p_list1 == 0 then true
          else let p1 = List.hd p_list1 in let p2 = List.hd p_list2 in
            if (equals p1 p2) then comp_lists (List.tl p_list1) (List.tl p_list2)
            else false in
          comp_lists p_list1 p_list2
    | _ -> false)
| Undef -> (match t2 with Undef -> true | _ -> false)

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
| Void -> "void"
| Function(rt, p_list) -> string_of_valid_type rt ^ "-func (" ^
                          String.concat ", " (List.map string_of_valid_type p_list) ^ ")"
| Undef -> "undefined"

let string_of_increment = function
  Incr_front -> "incr_front"
| Incr_back -> "incr_back"
| Decr_back -> "decr_back"
| Decr_front -> "decr_front"

let string_of_unop = function
  Neg -> "-"
| Not -> "!"
| Increment(i) -> "increment: " ^ string_of_increment i

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
| Noexpr -> "noexpr"
| Decl(t) -> "decl " ^ string_of_valid_type t
| Increment(i, l) -> string_of_increment i ^ " " ^ string_of_lval l

let rec string_of_stmt = function
  Assign(l, r) -> "assign { " ^ string_of_lval l ^ " = " ^ string_of_rval r ^ " };"
| Rval(r) -> "rval { " ^ string_of_rval r ^ " };"
| Return(r) -> "return " ^ string_of_rval r ^ ";"
| If(r, b1, b2) -> "if (" ^ string_of_rval r ^ ") then " ^ string_of_block b1 ^
                   " else " ^ string_of_block b2
| While(r, b) -> "while (" ^ string_of_rval r ^ ") " ^ string_of_block b
| For(s1, r, s2, b) -> "for (" ^ string_of_stmt s1 ^ "; " ^ string_of_rval r ^
                       "; " ^ string_of_stmt s2 ^ ")" ^ string_of_block b

and string_of_block (b:block) = "block " ^ string_of_int b.block_num ^ ": {\n" ^
    String.concat "" (List.map string_of_stmt b.stmts) ^
    "}\n"

let string_of_func_def (f:func_def) = "func_def " ^ f.id ^ ", type: " ^
  string_of_valid_type f.ret_type ^ " (" ^
  (List.fold_left (fun acc (t, i) -> acc ^ string_of_valid_type t ^ i) "" f.params) ^
  string_of_block f.body_block

let string_of_prog prog =
  string_of_func_def prog