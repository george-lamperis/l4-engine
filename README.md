l4-engine
=========

A UCI chess engine

I use Arena Chess GUI http://www.playwitharena.com/. Theoretically, this engine
should be able to interface with any program supporting the UCI protocol.


### Encoding ###
LERF encoding inspired/copied from
http://pages.cs.wisc.edu/~psilord/blog/data/chess-pages/rep.html
http://chessprogramming.wikispaces.com/Square+Mapping+Considerations

Each number corresponds to a bit, with 0 being the LSB and 63 being the MSB.
```
        a    b    c    d    e    f    g    h
      +----+----+----+----+----+----+----+----+
    8 | 56 | 57 | 58 | 59 | 60 | 61 | 62 | 63 | 8
      +----+----+----+----+----+----+----+----+
    7 | 48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 7
      +----+----+----+----+----+----+----+----+
    6 | 40 | 41 | 42 | 43 | 44 | 45 | 46 | 47 | 6
      +----+----+----+----+----+----+----+----+
    5 | 32 | 33 | 34 | 35 | 36 | 37 | 38 | 39 | 5
      +----+----+----+----+----+----+----+----+
    4 | 24 | 25 | 26 | 27 | 28 | 29 | 30 | 31 | 4
      +----+----+----+----+----+----+----+----+
    3 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 3
      +----+----+----+----+----+----+----+----+
    2 | 8  | 9  | 10 | 11 | 12 | 13 | 14 | 15 | 2
      +----+----+----+----+----+----+----+----+
    1 | 0  | 1  | 2  | 3  | 4  | 5  | 6  | 7  | 1
      +----+----+----+----+----+----+----+----+
        a    b    c    d    e    f    g    h
```

Some example bitboards:
```
a-file             0x0101010101010101
h-file             0x8080808080808080
1st rank           0x00000000000000FF
8th rank           0xFF00000000000000
a1-h8 diagonal     0x8040201008040201
h1-a8 antidiagonal 0x0102040810204080
light squares      0x55AA55AA55AA55AA
dark squares       0xAA55AA55AA55AA55
```

Neighbors compass rose:
```
    +----+----+----+
    | +7 | +8 | +9 |
    +----+----+----+
    | -1 |  0 | +1 |
    +----+----+----+
    | -9 | -8 | -7 |
    +----+----+----+
```
