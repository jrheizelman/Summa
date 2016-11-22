open Ast
open Sast

(* Handles hash table of symbols, type ((string, valid_type) Hashtable, int) *)

let print_table env =
  print_endline "Printing table.";
  Hashtbl.iter (fun key t -> print_endline (key ^ ": " ^ string_of_valid_type t)) (fst env)

(* each index in the array refers to a block and holds within it the scope of the parent block *)
let ancestor_scope = Array.make 1000 0

let rec symbol_table_get_id (id:string) env =
  let (table, scope) = env in
    let table_key = id ^ "_" ^ string_of_int scope in
      if Hashtbl.mem table table_key
        then Hashtbl.find table table_key
      else
        if scope = 0 then raise (Failure("Get id - Symbol " ^ id ^
                                      " not declared in current scope! (Scope: " ^
                                      string_of_int scope ^ ")"))
        else
          symbol_table_get_id id (table, ancestor_scope.(scope))

let symbol_table_add_id env (id:string) (t:valid_type) =
  let (table, scope) = env in
    let table_key = id ^ "_" ^ string_of_int scope in
      Hashtbl.add table table_key t;
      (table, scope)

let symbol_table_replace_id env (id:string) (t:valid_type) =
  let (table, scope) = env in
    let table_key = id ^ "_" ^ string_of_int scope in
      Hashtbl.replace table table_key t;
      (table, scope)
