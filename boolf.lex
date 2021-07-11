(* User Declarations*)

structure T = Tokens
type pos = (int*int)
type svalue = T.svalue
type ('a,'b) token = ('a,'b) T.token
type lexresult = (svalue, pos) token
val linp = ref 1;
val colp = ref 1;
val eolpos = ref 0;
exception UnknownToken;

val lexerError : string * int * int -> unit =
	fn (badToken ,lineNo, colNo) => (TextIO.print(
	"Unknown Token:"^(Int.toString lineNo)^":"^(Int.toString colNo)^":"^badToken^"\n");
	raise UnknownToken)

val eof = fn () => (
	T.EOF ((!linp, !colp), (!linp, !colp))
)

val keywords = [
	("PLUS", T.PLUS),
	("MINUS", T.MINUS),
	("TIMES", T.TIMES),
	("NEGATE", T.NEGATE),
	("EQUALS", T.EQUALS),
	("LESSTHAN", T.LESSTHAN),
	("GREATERTHAN", T.GREATERTHAN),
	("AND", T.AND),
	("OR", T.OR),
	("XOR", T.XOR),
	("NOT", T.NOT),
	("IMPLIES", T.IMPLIES),
	("if", T.IF),
	("then", T.THEN),
	("else", T.ELSE),
	("fi", T.FI),
	("let", T.LET),
	("in", T.IN),
	("end", T.END),
	("int", T.INTTYP),
	("bool", T.BOOLTYP),
	("fun", T.FUNDECL),
	("fn", T.LAMBDA)
]

fun findKeyword(text: string, pos1: pos, pos2: pos) =
	case List.find (fn (s, _) => s=text) keywords of
		SOME (str, tk) => tk(pos1, pos2)
		|NONE => T.ID(text, pos1, pos2)
%%
%header (functor BoolfLexFun(structure Tokens : Boolf_TOKENS));
%s BOOLF;
alpha = [a-zA-Z];
digit = [0-9];
ws = [\ \t];
eol = ("\013\010"|"\010"|"\013");
%%

<INITIAL>{ws}* => (linp:=1; eolpos:=0; YYBEGIN BOOLF; lex());
<BOOLF>{ws}+ => ( lex() );
<BOOLF>{eol} => (linp:=(!linp)+1; eolpos:=yypos+size yytext; colp:= 1; lex());
<BOOLF>{digit}+ => (colp:=yypos-(!eolpos)+1; T.NUM(
					List.foldl (fn (a,r) => ord(a) - ord(#"0") + 10*r) 0 (explode yytext),
					(!linp, !colp), (!linp, !colp)));
<BOOLF>"TRUE" => (colp:=yypos-(!eolpos)+1; T.BOOLCONST(true, (!linp, !colp), (!linp, !colp)));
<BOOLF>"FALSE" => (colp:=yypos-(!eolpos)+1; T.BOOLCONST(false, (!linp, !colp), (!linp, !colp)));
<BOOLF>{alpha}({alpha}|{digit})* => (colp:=yypos-(!eolpos)+1; findKeyword(yytext, (!linp, !colp), (!linp, !colp)));
<BOOLF>":" => (colp:=yypos-(!eolpos)+1; T.TYPDECL((!linp, !colp), (!linp, !colp)));
<BOOLF>"->" => (colp:=yypos-(!eolpos)+1; T.ARROW((!linp, !colp), (!linp, !colp)));
<BOOLF>"=>" => (colp:=yypos-(!eolpos)+1; T.DARROW((!linp, !colp), (!linp, !colp)));
<BOOLF>"=" => (colp:=yypos-(!eolpos)+1; T.ASSIGN((!linp, !colp), (!linp, !colp)));
<BOOLF>";" => (colp:=yypos-(!eolpos)+1; T.TERM((!linp, !colp), (!linp, !colp)));
<BOOLF>"(" => (colp:=yypos-(!eolpos)+1; T.LPAREN((!linp, !colp), (!linp, !colp)));
<BOOLF>")" => (colp:=yypos-(!eolpos)+1; T.RPAREN((!linp, !colp), (!linp, !colp)));
<BOOLF>. => (colp:=yypos-(!eolpos)+1; lexerError(yytext, !linp, !colp); T.ERROR((!linp, !colp), (!linp, !colp)));
