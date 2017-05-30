all: parser

LLVMCONFIG = llvm-config
CPPFLAGS = `$(LLVMCONFIG) --cppflags`
LDFLAGS = `$(LLVMCONFIG) --ldflags` -lpthread -ldl -lz -rdynamic
LIBS = `$(LLVMCONFIG) --libs`

parser.cpp: parser.y
	bison -d -o $@ $^

parser.hpp: parser.cpp

tokens.cpp: tokens.l
	lex -o tokens.cpp tokens.l

parser: main.cpp parser.cpp tokens.cpp node.h
	g++ -o $@ $(CPPFLAGS) parser.cpp tokens.cpp main.cpp node.h

clean:
	$(RM) -rf parser.cpp parser.hpp parser tokens.cpp $(OBJS)
