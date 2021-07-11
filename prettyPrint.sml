structure PrintSyntaxTree : 
     sig val print : TextIO.outstream * SyntaxTree.statement -> unit end =
struct

  structure S = SyntaxTree
  structure T = TypeChecker

fun print (outstream, s0) =
 let fun say s =  TextIO.output(outstream,s)
  fun sayln s= (say s; say "\n") 

  fun indent 0 = ()
    | indent i = (say "   "; indent(i-1))

  fun close d = (sayln ""; indent d; say ")")

  fun binopName S.Add = "Add"
    | binopName S.Sub = "Sub"
    | binopName S.Mul = "Mul"
    | binopName S.Equals = "Equals"
    | binopName S.And = "And"
    | binopName S.Or = "Or"
    | binopName S.Xor = "Xor"
    | binopName S.Implies = "Implies"
    | binopName S.LessThan = "LessThan"
    | binopName S.GreaterThan = "GreaterThan"
  fun unopName S.Negate = "Negate"
    | unopName S.Not = "Not"

  fun ty(S.IntTyp) = (say "IntTyp")
	| ty(S.BoolTyp) = (say "BoolTyp")
    | ty(S.Arrow(t1, t2)) = (say "Arrow("; ty(t1); say ", "; ty(t2); say ")")
    | ty(S.AssignTyp(t)) = (say "AssignTyp("; ty(t); say ")")

  fun stmt(S.FunDecl(name, argName, argTyp, retTyp, e, _), d) =
		(indent d; say "FunDecl(\""; say(name); say "\", \""; say(argName); say "\", "; ty(argTyp); say ", "; ty(retTyp); sayln ","; exp(e, d+1); close(d))
	| stmt(S.Exp(e, _), d) = (exp(e, d))
  and exp(S.VarExp (iden, _), d) = (indent d; say "VarExp(\""; say(iden); say "\")")
	| exp(S.NumExp (i, _), d) = (indent d; say "NumExp("; say(Int.toString i); say ")")
	| exp(S.BoolConst (b, _), d) = (indent d; say "BoolConst("; say(Bool.toString b); say ")")
	| exp(S.BinopExp(e1, b, e2, _), d) = 
		(indent d; say "BinopExp("; say(binopName b); sayln ","; exp(e1, d+1); sayln ","; exp(e2, d+1); close(d))
	| exp(S.UnopExp(u, e, _), d) = 
		(indent d; say "UnopExp("; say(unopName u); sayln ","; exp(e, d+1); close(d))
	| exp(S.IfExp(e1, e2, e3, _), d) = 
		(indent d; sayln "IfExp("; exp(e1, d+1); sayln ","; exp(e2, d+1); sayln ","; exp(e3, d+1); sayln ""; indent d;  say ")")
	| exp(S.LetExp(assign, e, _), d) = 
		(indent d; sayln "LetExp("; decl(assign, d+1); sayln ","; exp(e, d+1); close(d))
	| exp(S.LambdaAbst(argName, argTyp, retTyp, e, _), d) =
		(indent d; say "LambdaAbst(\""; say(argName); say "\", "; ty(argTyp); say ", "; ty(retTyp); sayln ","; exp(e, d+1); close(d)) 
	| exp(S.AppExp(e1, e2, _), d) =
		(indent d; sayln "AppExp("; exp(e1, d+1); sayln ","; exp(e2, d+1); close(d))
  and decl(S.VarDecl(name, e, _), d) =
		(indent d; say "VarDecl(\""; say(name); sayln "\","; exp(e, d+1); close(d))

 in  stmt(s0,0); sayln ""; TextIO.flushOut outstream
end

end


