MAIN     = Main
MODULES  = Syntax Lexer Parser Interpreter

OBJECTS  = $(MAIN:=.o)  $(MODULES:=.o)  $(TESTS:=.o)
HIFILES  = $(MAIN:=.hi) $(MODULES:=.hi) $(TESTS:=.hi)
BINARIES = $(MAIN)      $(MODULES)      $(TESTS)

all: $(MAIN)

$(MAIN):
	@ghc --make $(MAIN)

clean:
	@rm -f $(OBJECTS) $(HIFILES) $(BINARIES)
	@echo "CLEANED"
