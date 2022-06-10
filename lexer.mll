{
(* 補助的な変数、関数、型などの定義 *)
open Parser
}

(* 正規表現の略記 *)
(* [...] の中は character '...' でなくてはならない *)
let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let real = ['0'-'9' '.']
let lower = ['a'-'z']
let upper = ['A'-'Z']
let alpha = lower | upper

rule token = parse
| space+  { token lexbuf }      (* スペースは読み飛ばす *)
| "(*"    { comment lexbuf;
            token lexbuf }

| "+."     { PLUSDOT }
| "-."     { MINUSDOT }
| "*."     { TIMESDOT }
| "/."     { DIVIDEDOT }
| "+"     { PLUS }
| "-"     { MINUS }
| "*"     { TIMES }
| "/"     { DIVIDE }
| "mod"   { MOD }
| "("      { LPAREN }
| ")"      { RPAREN }
| "="      { EQUAL }
| "<>"     { NOTEQUAL }
| "<="     { LESSEQUAL }
| ">="     { GREATEREQUAL }
| "<"      { LESS }
| ">"      { GREATER }
| "if"     { IF }
| "then"   { THEN }
| "else"   { ELSE }
| "let"    { LET }
| "rec"    { REC }
| "in"     { IN }

| digit+                        (* 数字が１個以上 *)
          { NUMBER (int_of_string (Lexing.lexeme lexbuf)) }
| real+
          { REAL (float_of_string (Lexing.lexeme lexbuf))}
| lower (alpha | digit | '_')*  (* 小文字で始まる変数 *)
          { VARIABLE (Lexing.lexeme lexbuf) }
| eof     { EOF }               (* 入力終了 *)
| _       { failwith ("unknown token: " ^ Lexing.lexeme lexbuf) }

and comment = parse
| "*)"    { () }
| "(*"    { comment lexbuf;
            comment lexbuf }
| eof     { failwith "unterminated comment" }
| _       { comment lexbuf }