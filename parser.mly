%{
(* 補助的な変数、関数、型などの定義 *)
open Syntax
%}

/* トークンの定義 */
%token <int> NUMBER
%token <float> REAL
%token <string> VARIABLE

%token PLUS MINUS TIMES DIVIDE MOD
%token PLUSDOT MINUSDOT TIMESDOT DIVIDEDOT

%token EQUAL NOTEQUAL
%token LESS LESSEQUAL GREATER GREATEREQUAL

%token IF THEN ELSE
%token LET REC IN
%token APPLICATION

/*かっこ*/
%token LPAREN RPAREN

%token EOF
/* End of File: 入力の終わりを示す */

/* 非終端記号の型をここで宣言する */
%type <Syntax.t> start

/* 開始記号の定義 */
%start start


%nonassoc IN
%nonassoc THEN
%nonassoc ELSE
%nonassoc EQUAL NOTEQUAL
%nonassoc LESS LESSEQUAL GREATER GREATEREQUAL
%left MINUS MINUSDOT PLUS PLUSDOT
%left MOD
%left DIVIDE DIVIDEDOT TIMES TIMESDOT
%left APPLICATION
%nonassoc UNARY

%%
start:
| expr EOF
  { $1 }

simple_expr:
| NUMBER
  { Number ($1)}
| REAL
  { Real ($1)}
| VARIABLE
  { Variable ($1)}
| LPAREN expr RPAREN
  { $2 }

variables:
| VARIABLE
  { [$1, Type.gen_type()] }
| VARIABLE variables
  { ($1, Type.gen_type()) :: $2 }

expr:
| simple_expr
  { $1 }
| expr PLUS expr
  { Op ($1, Operator.Plus, $3)}
| expr MINUS expr
  { Op ($1, Operator.Minus, $3)}
| expr TIMES expr
  { Op ($1, Operator.Times, $3)}
| expr DIVIDE expr
  { Op ($1, Operator.Divide, $3)}
| expr MOD expr
  { Op ($1, Operator.Mod, $3)}
| expr PLUSDOT expr
  { Op ($1, Operator.PlusDot, $3)}
| expr MINUSDOT expr
  { Op ($1, Operator.MinusDot, $3)}
| expr TIMESDOT expr
  { Op ($1, Operator.TimesDot, $3)}
| expr DIVIDEDOT expr
  { Op ($1, Operator.DivideDot, $3)}
| MINUS expr %prec UNARY
  { Op (Number (0), Operator.Minus, $2) }
| IF expr EQUAL expr THEN expr ELSE expr
  { IfEqual ($2, $4, $6, $8)}
| IF expr NOTEQUAL expr THEN expr ELSE expr
  { IfEqual ($2, $4, $8, $6)}
| IF expr LESS expr THEN expr ELSE expr
  { IfLess ($2, $4, $6, $8)}
| IF expr LESSEQUAL expr THEN expr ELSE expr
  { IfLess ($4, $2, $8, $6)}
| IF expr GREATER expr THEN expr ELSE expr
  { IfLess ($4, $2, $6, $8)}
| IF expr GREATEREQUAL expr THEN expr ELSE expr
  { IfLess ($2, $4, $8, $6)}
| LET VARIABLE EQUAL expr IN expr
  { Let (($2, (Type.gen_type())), $4, $6) }
| LET REC VARIABLE variables EQUAL expr IN expr
  { LetRec (($3, Type.gen_type()), $4, $6, $8)}
| simple_expr exprs
  { Application ($1, $2) }

exprs:
| simple_expr
  { [$1] }
| simple_expr exprs
  { $1 :: $2 }
