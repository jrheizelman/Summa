open Ast
open Sast

let print_table env =
  Hashtbl.iter (fun key (l, t) -> print_endline (key ^ ": " ^ string_of_lval l)) (fst env)

(* each index in the array refers to a block and holds within it the scope of the parent block *)
let ancestor_scope = Array.make 1000 0

let rec symbol_table_access_lval (l:lval) env =
  let (table, scope) = env in
    let id = id_of_lval l in
      let table_key = id ^ "_" ^ string_of_int scope in
        if Hashtbl.mem table table_key
          then let (l, t) = Hashtbl.find table table_key in Access_lval_t(t, l)
        else
          if scope = 0 then raise (Failure("Get id - Symbol " ^ id ^
                                        " not declared in current scope! (Scope: " ^
                                        string_of_int scope ^ ")"))
          else
            symbol_table_access_lval l (table, ancestor_scope.(scope))

let symbol_table_add_lval (l:lval) (t:valid_type) env =
  let (table, scope) = env in
    let id = id_of_lval l in
      let table_key = id ^ "_" ^ string_of_int scope in
        try(
          let prev_t = type_of_rval_t (symbol_table_access_lval l env) in
            if not (prev_t = t) then
              print_endline (string_of_lval l ^ " WARNING: Overriding declaration with different type"));
            Hashtbl.replace table table_key (l, t);
            (table, scope)
        with Failure(_) ->
          Hashtbl.add table table_key (l, t);
          (table, scope)