{
  open Parser

  let unknown_character c = failwith (Printf.sprintf "Unknown character %c" c) 
}

(* Whitespace *)
let newline = ('\013'* '\010')
let blank = [' ' '\009' '\012']

(* Integers *)
let dec_lit = '-'? ['0'-'9'] ['0'-'9' '_']*

(* Symbols *)
let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let letter = lowercase | uppercase
let symbol = (lowercase | '_') (letter | '_' | '\'' |'?')*

rule read = parse
  | (blank | newline)+ { read lexbuf }

  | "--" { comment lexbuf }

  | "(" { LPAREN }
  | ")" { RPAREN }

  | "[" { LBRACK }
  | "]" { RBRACK }

  | "let" { LET }
  | "="   { EQ }
  | "in"  { IN }

  | symbol as s { SYMBOL s }

  | dec_lit as n { INT (int_of_string n) }

  | eof { EOF }

  | _ { unknown_character (Lexing.lexeme lexbuf).[0] }

and comment = parse
  | "\n" | eof { read lexbuf }
  | _ { comment lexbuf }
