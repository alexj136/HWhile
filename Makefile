MODULES  = Syntax Lexer Parser Interpreter

Main: $(MODULES:=.hs)
	@ghc --make Main

Lexer.hs: Lexer.x
	@alex Lexer.x

Parser.hs: Parser.y
	@happy Parser.y

clean:
	@rm -f $(MODULES:=.o) $(MODULES:=.hi) $(MODULES) \
		Main.o Main.hi Main Lexer.hs Parser.hs
	@echo "CLEANED"
