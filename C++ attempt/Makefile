all: summa

OBJS = parser.o  \
	tokens.o  \
    main.o    \

CPPFLAGS = -std=c++11
LDFLAGS = -lpthread -ldl -lz -lncurses -rdynamic

clean:
	$(RM) -rf parser.cpp *.hpp summa tokens.cpp $(OBJS) testall.log

parser.cpp: parser.y
	bison -d -o $@ $^

tokens.cpp: tokens.l
	flex -o $@ $^

%.o: %.cpp
	g++ -c $(CPPFLAGS) -o $@ $<

summa: $(OBJS)
	g++ -o $@ $(OBJS) $(LDFLAGS)

new: clean summa

test: new run_tests.sh
	./run_tests.sh
