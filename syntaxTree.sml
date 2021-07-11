signature SYNTAXTREE = 
sig
	type id = string
	
	datatype binop = Add | Sub | Mul | LessThan | GreaterThan | Equals | And | Or | Xor | Implies
	datatype unop  = Not | Negate

	datatype statement = Exp of exp*(int*int) | FunDecl of id*id*typ*typ*exp*(int*int)
	and exp = 
			 BinopExp of exp*binop*exp*(int*int) 
			|NumExp of int*(int*int)
			|VarExp of id*(int*int)
			|BoolConst of bool*(int*int)
			|UnopExp of unop*exp*(int*int)
			|IfExp of exp*exp*exp*(int*int)
			|LetExp of decl*exp*(int*int)
			|LambdaAbst of id*typ*typ*exp*(int*int)
			|AppExp of exp*exp*(int*int)
	and decl = VarDecl of id*exp*(int*int)
	and typ = IntTyp | BoolTyp | Arrow of typ*typ | AssignTyp of typ

	type program = statement list

end;

structure SyntaxTree :> SYNTAXTREE = 
struct
	type id = string
	
	datatype binop = Add | Sub | Mul | LessThan | GreaterThan | Equals | And | Or | Xor | Implies
	datatype unop  = Not | Negate

	datatype statement = Exp of exp*(int*int) | FunDecl of id*id*typ*typ*exp*(int*int)
	and exp = 
			 BinopExp of exp*binop*exp*(int*int) 
			|NumExp of int*(int*int)
			|VarExp of id*(int*int)
			|BoolConst of bool*(int*int)
			|UnopExp of unop*exp*(int*int)
			|IfExp of exp*exp*exp*(int*int)
			|LetExp of decl*exp*(int*int)
			|LambdaAbst of id*typ*typ*exp*(int*int)
			|AppExp of exp*exp*(int*int)
	and decl = VarDecl of id*exp*(int*int)
	and typ = IntTyp | BoolTyp | Arrow of typ*typ | AssignTyp of typ  

	type program = statement list

end;
