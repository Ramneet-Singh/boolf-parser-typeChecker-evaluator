open SyntaxTree
signature TYPECHECKER = 
sig
	type typEnvironment = (id*typ) list	
	exception brokenTypes

	val toString : typ -> string
	val checkTypsProgram : program*typEnvironment -> typ list
end

structure TypeChecker :> TYPECHECKER = 
struct

	type typEnvironment = (id*typ) list
	exception brokenTypes
	fun typEnvLookup(iden: id, typEnv: typEnvironment) : typ option =
		case List.find (fn (s,t) => s=iden) typEnv of
			SOME (var:id, t:typ) => SOME (t)
		   | _ => NONE
	fun typEnvAdd(iden: id, value: typ, typEnv: typEnvironment):typEnvironment = 
		(iden, value)::typEnv

	fun toString(IntTyp) = "int"
      | toString(BoolTyp) = "bool"
      | toString(Arrow(t1, t2)) = "("^toString(t1)^"->"^toString(t2)^")"
      | toString(AssignTyp(t)) = "Named Function Declaration of type: "^toString(t)

	(*Error Reporting Function. Print lin and col no and error message to stdOut and raise a brokenTypes exception*)
	fun typError(position:(int*int), message) = (TextIO.print("Type Error: " ^ Int.toString(#1position) ^": " ^	     Int.toString(#2position) ^ ": "^message^"\n"); raise brokenTypes)

	fun requiredTyp(Add) = IntTyp
       |requiredTyp(Sub) = IntTyp
       |requiredTyp(Mul) = IntTyp
       |requiredTyp(And) = BoolTyp
       |requiredTyp(Or) = BoolTyp
       |requiredTyp(Xor) = BoolTyp
       |requiredTyp(Implies) = BoolTyp
       |requiredTyp(LessThan) = IntTyp
       |requiredTyp(GreaterThan) = IntTyp

	fun checkTypExp(expression, typEnv) = 
		case expression of
  			 NumExp (_, _) => IntTyp
			|BoolConst (_, _) => BoolTyp
			|VarExp (var, startPos) => (
			case typEnvLookup(var, typEnv) of
				SOME t => t
               |NONE => typError (startPos, "Variable \""^var^"\" has not been declared")
			)
			|BinopExp(expression1, binaryOp, expression2, startPos) => (
			let
				val t1 = checkTypExp(expression1, typEnv)
				val t2 = checkTypExp(expression2, typEnv)
			in
			case (t1,t2,binaryOp) of
				 (IntTyp, IntTyp, Add) => IntTyp
				|(IntTyp, IntTyp, Sub) => IntTyp
				|(IntTyp, IntTyp, Mul) => IntTyp
				|(IntTyp, IntTyp, GreaterThan) => BoolTyp
				|(IntTyp, IntTyp, LessThan) => BoolTyp
				|(IntTyp, IntTyp, Equals) => BoolTyp
				|(BoolTyp, BoolTyp, Equals) => BoolTyp
				|(BoolTyp, BoolTyp, And) => BoolTyp
				|(BoolTyp, BoolTyp, Or) => BoolTyp
				|(BoolTyp, BoolTyp, Xor) => BoolTyp
				|(BoolTyp, BoolTyp, Implies) => BoolTyp
				| _ => (
					case binaryOp of
					Equals => typError(startPos, "Types of operands don't match required types in Binary Operator Expression\nTypes Present: "^toString(t1)^", "^toString(t2)^"\nRequired Types: Either int, int or bool, bool")
					| _ => 
					let
						val reqt = requiredTyp(binaryOp)
					in
						typError(startPos, "Types of operands don't match required types in Binary Operator Expression\nTypes Present: "^toString(t1)^", "^toString(t2)^"\nRequired Types: "^toString(reqt)^", "^toString(reqt))
					end		
				)			
			end
			)
			|UnopExp(unaryOp, expression, startPos) => (
			let
				val t = checkTypExp(expression, typEnv)
			in
			case (t, unaryOp) of
				 (IntTyp, Negate) => IntTyp
				|(BoolTyp, Not) => BoolTyp
				| _ => (
					let
						val reqt = case unaryOp of Not => BoolTyp | Negate => IntTyp
					in
						typError(startPos, "Type of operand doesn't match required type in Unary Operator Expression\nType Present: "^toString(t)^"\nRequired Type: "^toString(reqt))
					end
				)
			end
			)
			|IfExp(expression1, expression2, expression3, startPos) => (
			let
				val t1 = checkTypExp(expression1, typEnv)
				val t2 = checkTypExp(expression2, typEnv)
				val t3 = checkTypExp(expression3, typEnv)
			in
				if t1=BoolTyp then(
					if t2=t3 then
						t2
					else
					typError(startPos, "Two arms of if expression don't have equal types\nTypes Present: "^toString(t2)^", "^toString(t3)^"\nRequired Types: 'a, 'a for any valid type 'a")
				)else
				typError (startPos, "Condition in if expression doesn't have required type\nType Present:  "^toString(t1)^"\nRequired Type: bool")
			end
			)
			|LetExp(VarDecl(var, expression1, _), expression2, startPos) => (
			let
				val t = checkTypExp(expression1, typEnv)
			in
				checkTypExp(expression2, typEnvAdd(var, t, typEnv))
			end
			)
			|LambdaAbst(arg, argTyp, retTyp, expression, startPos) => (
				let
					val t = checkTypExp(expression, typEnvAdd(arg, argTyp, typEnv))
				in
					if t=retTyp then
						Arrow(argTyp, retTyp)
					else
					typError(startPos, "Right hand side of lambda function does not agree with function result type\nDeclared Return Type: " ^ toString(retTyp) ^ "\nChecked RHS Type: " ^ toString(t))
				end
			)
			|AppExp(expression1, expression2, startPos) => (
			let
				val t1 = checkTypExp(expression1, typEnv)
				val t2 = checkTypExp(expression2, typEnv)		
			in
				case t1 of
				 Arrow(ta, tb) => (
					if ta=t2 then tb
					else typError(startPos, "Types of Formal and Actual Parameters don't match in Function Application\nFormal Parameter Type: "^toString(ta)^"\nActual Parameter Type: "^toString(t2))
				)
				| _ => typError(startPos, "First Expression is not a function type in function application expression\nActual Type: "^toString(t1)^"\nRequired Type: "^toString(t2)^" -> 'a for any valid type 'a")
			end
			)

	fun checkTypStatement(stmt: statement, typEnv: typEnvironment): typ = 
		case stmt of 
			FunDecl(f, x, t1, t2, expression, startPos) => (
				let
					val returnTyp = checkTypExp(expression, typEnvAdd(x, t1, typEnvAdd(f, Arrow(t1, t2), typEnv)))
				in
					if returnTyp=t2 then AssignTyp(Arrow(t1, t2))
					else typError(startPos, "Right hand side of function declaration does not agree with function result type\nDeclared Return Type: " ^ toString(t2) ^ "\nChecked RHS Type: " ^ toString(returnTyp))
				end
			)
		    |Exp(expression, _) => checkTypExp(expression, typEnv)

	fun checkTypsProgram (stmtList: program, typEnv: typEnvironment) = 
		case stmtList of 
			[] => []
		   |stmt::stmtTail => (
				let 
					val t = checkTypStatement(stmt, typEnv)
				in
					case stmt of 
						Exp(expression) => (
							t::checkTypsProgram(stmtTail, typEnv)
						)
					   |FunDecl(f, x, t1, t2, expression, startPos) => (
							if(t = AssignTyp(Arrow(t1, t2))) then							
							t::checkTypsProgram(stmtTail, typEnvAdd(f, Arrow(t1, t2), typEnv))
							else typError(startPos, "Type of Function Declaration does not match the types given")
						)
				end			
			)
end
