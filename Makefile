MODULES  = Syntax Lexer Parser Interpreter

Main: $(MODULES:=.hs) Main.hs
	@ghc --make Main -o hwhile
	@echo "Running Unit Tests"
	@runhaskell UnitTests.hs

Lexer.hs: Lexer.x
	@echo "Generating Lexer"
	@alex Lexer.x

Parser.hs: Parser.y
	@echo "Generating Parser"
	@happy Parser.y

clean:
	@rm -f $(MODULES:=.o) $(MODULES:=.hi) $(MODULES) \
		Main.o Main.hi hwhile
	@echo "CLEANED"
