l4-engine
=========

A UCI chess engine

I use Arena Chess GUI http://www.playwitharena.com/. Theoretically, this engine
should be able to interface with any program supporting the UCI protocol.


### Encoding ###
LERF encoding inspired/copied from
http://pages.cs.wisc.edu/~psilord/blog/data/chess-pages/rep.html
http://chessprogramming.wikispaces.com/Square+Mapping+Considerations

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

Neighbors compass rose
```
    +----+----+----+
    | +7 | +8 | +9 |
    +----+----+----+
    | -1 |  0 | +1 |
    +----+----+----+
    | -9 | -8 | -7 |
    +----+----+----+
```
