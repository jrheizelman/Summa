all: parser

OBJS = parser.o  \
       main.o    \
       tokens.o  \

CPPFLAGS = -std=c++11
LDFLAGS = -lpthread -ldl -lz -lncurses -rdynamic

clean:
	$(RM) -rf parser.cpp parser.hpp parser tokens.cpp $(OBJS)

parser.cpp: parser.y
	bison -d -o $@ $^

parser.hpp: parser.cpp

tokens.cpp: tokens.l parser.hpp
	flex -o $@ $^

%.o: %.cpp
	g++ -c $(CPPFLAGS) -o $@ $<


parser: $(OBJS)
	g++ -o $@ $(OBJS) $(LDFLAGS)

test: parser example.txt
	cat example.txt | ./parser
