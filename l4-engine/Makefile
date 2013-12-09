CC=cc
CFLAGS=-Wall -std=c99

%.o: %.c 
	$(CC) -c -o $@ $< $(CFLAGS)

l4engine: bitboard.o search.o main.o
	$(CC) -o $@ bitboard.o search.o main.o $(CFLAGS)
