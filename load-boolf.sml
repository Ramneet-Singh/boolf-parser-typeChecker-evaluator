structure Boolf : sig
                  val parse : string -> unit
                  end = 
struct

	structure BoolfLrVals = BoolfLrValsFun(structure Token = LrParser.Token)
	structure BoolfLex = BoolfLexFun(structure Tokens = BoolfLrVals.Tokens);
	structure BoolfParser =
		  Join(structure LrParser = LrParser
     	       	structure ParserData = BoolfLrVals.ParserData
     	       	structure Lex = BoolfLex)
	structure PP = PrintSyntaxTree
	exception SyntaxError;     
	fun invoke lexstream =
	    	     	let fun print_error (s:string,(lin1:int, col1:int),_) =
			    	TextIO.print("Syntax Error:" ^ Int.toString(lin1) ^ ":" ^ Int.toString(col1) ^ ":" ^ s ^ "\n")
			in
		    	BoolfParser.parse(0,lexstream,print_error,())
			end

	fun streamToLexer inStream =
    	let 
    		val lexer=  BoolfParser.makeLexer (fn (n:int) => (if TextIO.endOfStream inStream then "" else TextIO.inputN(inStream, n)))
    	in
		lexer
    	end	
		
	fun parse (fileName) =
    	let 
			val inStream = TextIO.openIn fileName
			val lexer = streamToLexer inStream
			val dummyEOF = BoolfLrVals.Tokens.EOF((0,0), (0, 0))
    		val (revResult, lexer) = invoke lexer
			val result =  rev revResult		
		handle BoolfParser.ParseError => raise SyntaxError
		val (nextToken, lexer) = BoolfParser.Stream.get lexer
		val _ = TextIO.closeIn inStream
    	in
        	if BoolfParser.sameToken(nextToken, dummyEOF) then (
				TextIO.print("################ TYPE CHECKING ####################\n\n");			

			List.map (fn t => (TextIO.print("---------------------\n"); TextIO.print(TypeChecker.toString(t)^"\n"); TextIO.print("---------------------\n\n"))) 
						(TypeChecker.checkTypsProgram(result, []));

			TextIO.print("################ EVALUATED VALUES ####################\n\n");

			List.map (fn v => (TextIO.print("---------------------\n"); Evaluator.printVal(v); TextIO.print("---------------------\n\n")))
						(Evaluator.evalProgram (result, []));

			TextIO.print("################ ABSTRACT SYNTAX TREES ####################\n\n");

			List.map (fn v => (TextIO.print("---------------------\n"); PP.print(TextIO.stdOut, v); TextIO.print("---------------------\n\n"))) 
						(result); ())


 		else (TextIO.print("Warning: Unconsumed input \n"); (

			TextIO.print("################ TYPE CHECKING ####################\n\n");			

			List.map (fn t => (TextIO.print("---------------------\n"); TextIO.print(TypeChecker.toString(t)^"\n"); TextIO.print("---------------------\n\n"))) 
						(TypeChecker.checkTypsProgram(result, []));

			TextIO.print("################ EVALUATED VALUES ####################\n\n");

			List.map (fn v => (TextIO.print("---------------------\n"); Evaluator.printVal(v); TextIO.print("---------------------\n\n")))
						(Evaluator.evalProgram (result, []));

			TextIO.print("################ ABSTRACT SYNTAX TREES ####################\n\n");

			List.map (fn v => (TextIO.print("---------------------\n"); PP.print(TextIO.stdOut, v); TextIO.print("---------------------\n\n"))) 
						(result)); ()
		)
    	end

end

exception MissingCmdLineArgs;
val (inFile::_) = case CommandLine.arguments() of [] => (raise MissingCmdLineArgs) | (hd::tl) => (hd::tl);
val _ = Boolf.parse(inFile);
