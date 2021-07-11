all: boolf.lex boolf.yacc boolf.mlb
	mllex boolf.lex
	mlyacc boolf.yacc
	mlton boolf.mlb
clean:
	rm boolf.lex.sml boolf.yacc.sig boolf.yacc.sml boolf
