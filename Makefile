all: summa

OBJS = parser.o  \
	tokens.o  \
    main.o    \

CPPFLAGS = -std=c++11
LDFLAGS = -lpthread -ldl -lz -lncurses -rdynamic

clean:
	$(RM) -rf parser.cpp parser.hpp summa tokens.cpp $(OBJS) tokens.hpp

parser.cpp: parser.y
	bison -d -o $@ $^

parser.hpp: parser.cpp

tokens.cpp: tokens.l
	flex -o $@ $^

%.o: %.cpp
	g++ -c $(CPPFLAGS) -o $@ $<

summa: $(OBJS)
	g++ -o $@ $(OBJS) $(LDFLAGS)