open Unix

type action = Ast | Help

let usage (name:string) =
  "Usage: " ^ name ^ " [-a] file.sum"

let get_compiler_path (path:string) =
try
let i = String.rindex path '/' in
String.sub path 0 i
with _ -> "."
let _ =
  let action =
  if Array.length Sys.argv > 1 then
    (match Sys.argv.(1) with
        "-a" -> if Array.length Sys.argv == 3 then Ast else Help
      | _ -> Help)
  else Help in

  match action with
      Help -> print_endline (usage Sys.argv.(0))
    | Ast ->
      let input = open_in Sys.argv.(2) in
      let lexbuf = Lexing.from_channel input in
      let program = Parser.program Scanner.token lexbuf in
      match action with
          Ast -> let prog_string = Ast.string_of_prog program
                 in print_endline prog_string
        | Help -> print_endline (usage Sys.argv.(0)) (* impossible case *)