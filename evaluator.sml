signature EVALUATOR =
sig
	type environment = (id*exp) list
	val printVal : statement -> unit	
	val evalProgram : program*environment -> statement list
end

structure Evaluator : EVALUATOR = 
struct
	structure PP = PrintSyntaxTree
	structure TC = TypeChecker

	fun printVal(stmt) = 
		case stmt of
		 FunDecl(name, _, t, rt, _, _) => (
			TextIO.print(
			"Function Declaration with name \""^name^"\", type: "^TC.toString(Arrow(t, rt))^" and reduced AST:\n"
			);	PP.print(TextIO.stdOut, stmt)
		)
		|Exp(e, start) => (
			case e of
			 NumExp(i, _) => TextIO.print ((Int.toString i)^" : int\n")
			|BoolConst(b, _) => TextIO.print ((Bool.toString b)^" : bool\n")
			|LambdaAbst(_, t, rt, _, _) => (
			TextIO.print(
			"Lambda Function with type: "^TC.toString(Arrow(t, rt))^" and reduced AST:\n"
			); PP.print(TextIO.stdOut, Exp(e, start))
			)
			| _ => (TextIO.print("NOT One of These 3\n"); PP.print(TextIO.stdOut, Exp(e, start)))		
		)

	type environment = (id*exp) list
	fun envAdd(iden: id, e: exp, env: environment):environment = (iden, e)::env
	fun envLookup(iden: id, env: environment): exp option = 
		case List.find (fn (var, e) => var=iden) env of
		 SOME (x, e) => SOME(e)
		|NONE => NONE
	fun evalExp(expression: exp, env: environment):exp = 
		case expression of
		 NumExp (i, _) => expression
		|BoolConst (b, _) => expression
		|VarExp (x, _) => (
			case envLookup(x, env) of SOME(value) => value | NONE => expression
		)
		|BinopExp(expression1, binaryOp, expression2, startPos) => (
			let
				val e1 = evalExp(expression1, env)
				val e2 = evalExp(expression2, env)
			in
				case (e1, e2, binaryOp) of
				 (NumExp (i1, _), NumExp (i2, _), Add) => NumExp (i1+i2, startPos)
				|(NumExp (i1, _), NumExp (i2, _), Sub) => NumExp (i1-i2, startPos)
				|(NumExp (i1, _), NumExp (i2, _), Mul) => NumExp (i1*i2, startPos)
				|(NumExp (i1, _), NumExp (i2, _), LessThan) => BoolConst (i1<i2, startPos)
				|(NumExp (i1, _), NumExp (i2, _), GreaterThan) => BoolConst (i1>i2, startPos)
				|(NumExp (i1, _), NumExp (i2, _), Equals) => BoolConst (i1=i2, startPos)
				|(BoolConst (i1, _), BoolConst (i2, _), Equals) => BoolConst (i1=i2, startPos)
				|(BoolConst (i1, _), BoolConst (i2, _), And) => BoolConst (i1 andalso i2, startPos)
				|(BoolConst (i1, _), BoolConst (i2, _), Or) => BoolConst (i1 orelse i2, startPos)
				|(BoolConst (i1, _), BoolConst (i2, _), Xor) => BoolConst ((i1 andalso (not i2)) orelse ((not i1) andalso i2), startPos)
				|(BoolConst (i1, startPos), BoolConst (i2, _), Implies) => BoolConst ((not i1) orelse (i2), startPos)
				| _ => BinopExp(e1, binaryOp, e2, startPos)
			end
		)
		|UnopExp(unaryOp, expression1, startPos) => (
			let
				val e = evalExp(expression1, env)
			in
				case (unaryOp, e) of
				 (Not, BoolConst (b, _)) => BoolConst (not b, startPos)
				|(Negate, NumExp (i, _)) => NumExp (~i, startPos)
				| _ => UnopExp(unaryOp, e, startPos)
			end
		)
		|IfExp(expression1, expression2, expression3, startPos) => (
			if expression2=expression3 then evalExp(expression2, env)
			else (
				let
					val e1 = evalExp(expression1, env)
				in
					case e1 of
					 BoolConst (b, _) => (if b then evalExp(expression2, env) else evalExp(expression3, env))
					| _ => IfExp(e1, evalExp(expression2, env), evalExp(expression3, env), startPos)				
				end
			)
		)
		|LetExp(VarDecl(var, expression1, startPos1), expression2, startPos2) => (
			let
				val e1 = evalExp(expression1, env)
			in
				evalExp(expression2, envAdd(var, e1, env))
			end
		)
		|LambdaAbst(arg, argTyp, retTyp, expression1, startPos) => (
			let
				val e = evalExp(expression1, envAdd(arg, VarExp(arg, startPos), env))
			in
				LambdaAbst(arg, argTyp, retTyp, e, startPos)
			end
		)
		|AppExp(expression1, expression2, startPos) => (
			let
				val e1 = evalExp(expression1, env)				
				val e2 = evalExp(expression2, env)
			in
				case e1 of
				 LambdaAbst(arg, argTyp, retTyp, expression3, _) => (
					evalExp(expression3, envAdd(arg, e2, env))
				)
				| _ => AppExp(e1, e2, startPos)				
			end
		)
	fun evalProgram([], _) = []
      | evalProgram(stmt::stmtList, env) = 
		case stmt of
		FunDecl(name, argName, argTyp, retTyp, expression, startPos) => (
			let
				val normalForm = evalExp(expression, envAdd(argName, VarExp(argName, startPos), envAdd(name, VarExp (name, startPos), env)))
				val newEnv = envAdd(name, LambdaAbst(argName, argTyp, retTyp, normalForm, startPos), env)
			in
				FunDecl(name, argName, argTyp, retTyp, normalForm, startPos)::evalProgram(stmtList, newEnv)
			end
		)
	   |Exp(expression, startPos) => Exp(evalExp(expression, env), startPos)::evalProgram(stmtList, env)  
end
