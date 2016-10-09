open Ast
open Sast

(* Symbol table made of a map containing pairs of String: ast.decl pairs *)
(* fst of each pair is the string of decl, snd of each pair is the decl type *)

(*module TagMap = Map.Make(String)*)

(* each index in the array refers to a block and holds within it the scope of the parent block *)
let ancestor_scope = Array.make 1000 0

let symbol_table_add_lval (l:lval) (t:valid_type) env =
  let (table, scope) = env in
    match l with
      Id(id) -> let table_key = id ^ "_" ^ string_of_int scope in
        Hashtbl.add table table_key (l, t);
        (table, scope)