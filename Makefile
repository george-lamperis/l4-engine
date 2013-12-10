CC=cc
CFLAGS=-Wall -std=c99

CXX=g++
CXXFLAGS=-Wall

OBJ_DIR = obj
SRC_DIR = l4engine

OBJS = obj/bitboard.o \
		obj/main.o

TESTOBJS = obj/test_search.o \
		obj/test_uci.o \
		obj/test_main.o \


all: dir l4engine tests.out

dir:
	mkdir -p obj

# l4engine project
obj/%.o: l4-engine/%.c
	$(CC) -c -o $@ $< $(CFLAGS)

l4engine: $(OBJS)
	$(CC) -o $@ $^ $(CFLAGS)

# Test Cases
obj/%.o: tests/%.cpp
	$(CXX) -c -o $@ $< $(CXXFLAGS) -I l4-engine -lgtest

tests.out: l4engine $(TESTOBJS)
	$(CXX) -o $@ $^ $(CXXFLAGS) -I l4-engine -lgtest

clean:
	rm -rf $(OBJ_DIR)
	rm -f l4engine
	rm -f tests.out
