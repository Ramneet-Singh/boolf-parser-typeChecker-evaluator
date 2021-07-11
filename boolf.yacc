(* User Declarations *)
structure ST = SyntaxTree
%%
%name Boolf
%term TERM | IF | THEN | ELSE | FI
| AND | OR | XOR | EQUALS | NOT
| PLUS | MINUS | TIMES | NEGATE | LESSTHAN | GREATERTHAN
| ASSIGN | LET | IN | END | TYPDECL | INTTYP | BOOLTYP | ARROW | DARROW 
| FUNDECL | LAMBDA
| NUM of int | BOOLCONST of bool | ID of string | LPAREN | RPAREN | IMPLIES
| EOF | ERROR | IFTHENELSE | UNOP
%nonterm 
 program of ST.program | stmt_list of ST.program
|statement of ST.statement
|expression of ST.exp
|unop of ST.unop
|typ of ST.typ

%pos (int*int)
%eop EOF
%noshift EOF
%nonassoc ASSIGN
%right DARROW
%right IFTHENELSE
%right IMPLIES
%left AND 
%left OR XOR 
%left EQUALS
%left LESSTHAN GREATERTHAN
%left PLUS MINUS
%left TIMES
%right UNOP
%right ARROW
%start program
%%
program   : stmt_list				(( stmt_list ))
stmt_list : stmt_list TERM statement (( statement::stmt_list ))
          | statement               (( statement::[] ))
statement : expression				(( ST.Exp(expression, expressionleft) ))
		  | FUNDECL ID LPAREN ID TYPDECL typ RPAREN TYPDECL typ DARROW expression
                                    (( ST.FunDecl(ID1, ID2, typ1, typ2, expression, FUNDECLleft) ))
expression : expression AND expression (( ST.BinopExp(expression1, ST.And, expression2, expression1left) ))
           | expression OR expression (( ST.BinopExp(expression1, ST.Or, expression2, expression1left) ))
           | expression XOR expression (( ST.BinopExp(expression1, ST.Xor, expression2, expression1left) ))
           | expression EQUALS expression (( ST.BinopExp(expression1, ST.Equals, expression2, expression1left) ))
           | expression IMPLIES expression (( ST.BinopExp(expression1, ST.Implies, expression2, expression1left) ))
           | expression PLUS expression (( ST.BinopExp(expression1, ST.Add, expression2, expression1left) ))
           | expression MINUS expression (( ST.BinopExp(expression1, ST.Sub, expression2, expression1left) ))
           | expression TIMES expression (( ST.BinopExp(expression1, ST.Mul, expression2, expression1left) ))
           | expression GREATERTHAN expression (( ST.BinopExp(expression1, ST.GreaterThan, expression2, 	expression1left) ))
           | expression LESSTHAN expression (( ST.BinopExp(expression1, ST.LessThan, expression2, expression1left) ))
		   | unop expression %prec UNOP  (( ST.UnopExp(unop, expression, unopleft) ))
           | IF expression THEN expression ELSE expression FI  %prec IFTHENELSE 
                                    (( ST.IfExp(expression1, expression2, expression3, IFleft) ))
           | LET ID ASSIGN expression IN expression END 
                                    (( ST.LetExp(ST.VarDecl(ID, expression1, IDleft), expression2, LETleft) ))
		   | LAMBDA LPAREN ID TYPDECL typ RPAREN TYPDECL typ DARROW expression 
                                    (( ST.LambdaAbst(ID, typ1, typ2, expression, LAMBDAleft) ))
		   | LPAREN expression expression RPAREN
	                                (( ST.AppExp(expression1, expression2, LPARENleft) ))
		   | LPAREN expression RPAREN (( expression ))
		   | NUM                    (( ST.NumExp(NUM, NUMleft) ))
           | BOOLCONST              (( ST.BoolConst(BOOLCONST, BOOLCONSTleft) ))
		   | ID                     (( ST.VarExp(ID, IDleft) ))
unop      : NEGATE                  (( ST.Negate ))
          | NOT                     (( ST.Not ))
typ       : INTTYP                  (( ST.IntTyp ))
          | BOOLTYP                 (( ST.BoolTyp ))
		  | typ ARROW typ           (( ST.Arrow(typ1, typ2) ))
		  | LPAREN typ RPAREN       (( typ ))
