{ open Parser }

let digit = ['0'-'9']
let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule token = parse
[' ' '\t' 'r' '\n'] { token lexbuf }
| "/*" { comment lexbuf }
| '(' { LPAREN }
| ')' { RPAREN }
| '+' { PLUS }
| '*' { TIMES }
| '-' { MINUS }
| '/' { DIVIDE }
| "==" { EQ }
| "!=" { NEQ }
| "<=" { LEQ }
| ">=" { GEQ }
| '<' { LTHAN }
| '>' { GTHAN }
| "int" { INT }
| "double" { DOUBLE }
| "bool" { BOOL }
| ("true" | "false") as lxm	{ BOOLLIT(bool_of_string lxm) }
| ['0'-'9']+'.'['0'-'9']+ as lxm { DOUBLIT(float_of_string lxm)}
| ['0'-'9']+ as lxm { INTLIT(int_of_string lxm)}
| eof { EOF }
| _ as char { raise(Failure("illegal character " ^ Char.escaped char))}

and comment = parse
"*/" { token lexbuf }
| _ { comment lexbuf }
