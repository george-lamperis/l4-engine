CC=cc
CFLAGS=-Wall -std=c99

CXX=g++
CXXFLAGS=-Wall

OBJS = obj/bitboard.o \
		obj/uci.o \

TESTOBJS = obj/test_search.o \
		obj/test_uci.o \
		obj/test_main.o \


all: dir l4engine test

dir:
	mkdir -p obj

# l4engine project
obj/%.o: l4-engine/%.c
	$(CC) -c -o $@ $< $(CFLAGS)

l4engine: $(OBJS)
	$(CC) -o $@ l4-engine/main.c $^ $(CFLAGS)

# Test Cases
obj/%.o: tests/%.cpp
	$(CXX) -c -o $@ $< $(CXXFLAGS) -I l4-engine

test: l4engine $(TESTOBJS)
	$(CXX) -o $@ $(TESTOBJS) $(OBJS) $(CXXFLAGS) -I l4-engine -lgtest -lpthread

clean:
	rm -rf obj
	rm -f l4engine
	rm -f test

