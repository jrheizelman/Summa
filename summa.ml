open Unix

type action = Ast | Help | SAnalysis (*| Compile*)

let usage (name:string) =
  "Usage: " ^ name ^ " OPTION FILE\n" ^
  " -a\t\tPrint AST of source\n" ^
  " -s\t\tRun Semantic Analysis over source\n"
  (* " -c\t\tCompile source\n" *)

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
      | "-s" -> if Array.length Sys.argv == 3 then SAnalysis else Help
      (* | "-c" -> if Array.length Sys.argv == 3 then Compile else Help *)
      | _ -> Help)
  else Help in

  match action with
      Help -> print_endline (usage Sys.argv.(0))
    | _ -> let input = open_in Sys.argv.(2) in
        let lexbuf = Lexing.from_channel input in
          let program = Parser.program Scanner.token lexbuf in
            match action with
              Ast -> let prog_string = Ast.string_of_prog program in
                print_endline prog_string
            | SAnalysis -> let checked = Semantic_check.check_program program in
                ignore checked; print_endline "Passed Semantic Analysis."
            (* | Compile ->
                let (table, scope) = Semantic_check.check_program program in
                  Codegen.write_code "summa" scope; print_endline "Compiled." *)
            | _ -> () (* Impossible case, help caught earlier *)