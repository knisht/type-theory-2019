HC=ghc
SOURCES=src/Main.hs src/Grammar.hs src/Reduction.hs
GEN_SOURCES=src/Lexer.x src/Parser.y
GENERATED=src/Lexer.hs src/Parser.hs
PACKAGE=hw2.zip


rwildcard=$(foreach d,$(wildcard $1*),$(call rwildcard,$d/,$2) $(filter $(subst *,%,$2),$d))

BUILD_FILES=$(call rwildcard, , *.o *.hi *.a)



.PHONY: pack all run clean

all: parser

run: parser
	./parser

clean:
	rm -rf src/*.o src/*.hi
	rm -rf $(GENERATED)
	rm -rf $(BUILD_FILES)
	rm -f parser


parser: $(GENERATED) $(SOURCES)
	$(HC) -O2 -i./src ./src/Main.hs -o parser 

prof: $(GENERATED) $(SOURCES)
	$(HC) -prof -fprof-auto -rtsopts -O2 -i./src ./src/Main.hs -o parser

$(GENERATED): $(GEN_SOURCES) $(SOURCES)
	alex src/Lexer.x -o src/Lexer.hs
	happy src/Parser.y -o src/Parser.hs

pack: $(GENERATED)
	zip $(PACKAGE) -r Makefile src lib
