type bop = Equal | Neq | Leq | Geq | Greater | Less | And | Or
| Add | Sub | Mult | Div | Mod

type increment = Incr_front | Incr_back | Decr_front | Decr_back

type unop = Neg | Not | Increment of increment

type monotype = Int | Bool | Double | Char | String | Void

(* TODO add user-groups i.e. interfaces with condition_list and supergroup *)
type grouping = Num | CharString

type polytype =
  Reference of string
| Conditioned of (valid_type -> bool) list
| Function of (valid_type * string) list * valid_type
| Grouping of grouping

and valid_type =
  Mono of monotype * grouping list
| Poly of polytype

type lval =
  Id of string
(* | Access_arr of lval * rval *)

and rval =
  Int_lit of int
| Bool_lit of bool
| Double_lit of float
| Binop of rval * bop * rval
| Unop of unop * rval
| Access_lval of lval
| Noexpr
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

(* let rec equals (t1:valid_type) (t2:valid_type) = match t1 with
  Int -> (match t2 with Int -> true | _ -> false)
| Bool -> (match t2 with Bool -> true | _ -> false)
| Double -> (match t2 with Double -> true | _ -> false)
| Char -> (match t2 with Char -> true | _ -> false)
| String -> (match t2 with String -> true | _ -> false) *)

let rec id_of_lval = function
  Id(id) -> id

(*************************
**** PRINT AST **********
*************************)

let string_of_bop (b:bop) = match b with
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

let string_of_monotype (m:monotype) = match m with
  Int -> "int"
| Bool -> "bool"
| Double -> "double"
| Void -> "void"
| Char -> "char"
| String -> "string"

let string_of_grouping (g:grouping) = match g with
  Num -> "num"
| CharString -> "char-string"

(* TODO write a print function for valid_type to bool function list *)
let rec string_of_polytype (p:polytype) = match p with
  Reference(s) -> "ref_to_type: " ^ s
| Conditioned(check_list) -> "conditioned_type"
| Function(t_list, t) -> "fun(" ^
    String.concat ", "
      (List.map
        (fun entry -> string_of_valid_type (fst entry) ^ " " ^ snd entry)
        t_list)
    ^ ") -> " ^ string_of_valid_type t
| Grouping(g) -> "group" ^ string_of_grouping g

and string_of_valid_type (t:valid_type) = match t with
  Mono(m, g) -> string_of_monotype m ^ ", g:" ^
    String.concat "," (List.map string_of_grouping g)
| Poly(p) -> string_of_polytype p

let string_of_increment (i:increment) = match i with
  Incr_front -> "incr_front"
| Incr_back -> "incr_back"
| Decr_back -> "decr_back"
| Decr_front -> "decr_front"

let string_of_unop (u:unop) = match u with
  Neg -> "-"
| Not -> "!"
| Increment(i) -> "increment: " ^ string_of_increment i

let rec string_of_lval (l:lval) = match l with
  Id(i) -> "id " ^ i

and string_of_rval (r:rval) = match r with
  Int_lit(i) -> "int_lit " ^ string_of_int i
| Bool_lit(b) -> "bool_lit " ^ string_of_bool b
| Double_lit(d) -> "double_lit " ^ string_of_float d
| Binop(r1, b, r2) -> "Binop { " ^ string_of_rval r1 ^ " " ^ string_of_bop b ^
    " " ^ string_of_rval r2 ^ " }"
| Unop(u, r) -> "Unop { " ^ string_of_unop u ^ " " ^ string_of_rval r ^ " }"
| Access_lval(l) -> "access " ^ string_of_lval l
| Noexpr -> "noexpr"
| Increment(i, l) -> string_of_increment i ^ " " ^ string_of_lval l

let rec string_of_stmt (s:stmt) = match s with
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
