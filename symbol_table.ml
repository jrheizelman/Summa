open Ast
open Sast

(* Handles hash table of symbols. Env is type ((string, valid_type) Hashtable, int) *)

let print_table s_table =
  print_endline "Printing table.";
  Hashtbl.iter (fun key t -> print_endline (key ^ ": " ^ string_of_valid_type t)) (s_table)

let rec symbol_table_get_id (id:string) s_table =
  if Hashtbl.mem s_table id
    then Hashtbl.find s_table id
  else
    raise (Failure("Get id - Symbol " ^ id ^
                   " not declared in current scope!"))

let symbol_table_add_id s_table (id:string) (t:valid_type) =
  Hashtbl.add s_table id t;
  s_table

let symbol_table_replace_id s_table (id:string) (t:valid_type) =
  Hashtbl.replace s_table id t;
  s_table
