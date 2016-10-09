open Ast
open Sast

let print_table env =
  Hashtbl.iter (fun key (l, t) -> print_endline (key ^ ": " ^ string_of_lval l)) (fst env)

(* each index in the array refers to a block and holds within it the scope of the parent block *)
let ancestor_scope = Array.make 1000 0

let symbol_table_add_lval (l:lval) (t:valid_type) env =
  let (table, scope) = env in
    match l with
      Id(id) -> let table_key = id ^ "_" ^ string_of_int scope in
        Hashtbl.add table table_key (l, t);
        print_table (table, scope);
        print_endline "";
        (table, scope)

let rec symbol_table_access_lval (l:lval) env =
  let (table, scope) = env in
    match l with
      Id(id) -> let table_key = id ^ "_" ^ string_of_int scope in
        if Hashtbl.mem table table_key
          then let (l, t) = Hashtbl.find table table_key in Access_lval_t(t, l)
        else
          if scope = 0 then raise (Failure("Get id - Symbol " ^ id ^
                                        " not declared in current scope! (Scope: " ^
                                        string_of_int scope ^ ")"))
          else
            symbol_table_access_lval l (table, ancestor_scope.(scope))