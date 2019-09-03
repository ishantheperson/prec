# prec - Primitive recursion interpreter 

## Dependencies/Compilation

No dependencies are required besides Haskell itself.
To compile, simply run `make`.

## Usage

Interactive mode:

```
% ./prec
Prec> add = Prec[S o P 2 3, P 1 1]
f: N^2 -> N^1
f = Prec[(S o Proj(2, 3)), Proj(1, 1)]
Prec> Prec[add o (P 2 3, P 3 3), 0]
f: N^2 -> N^1
f = Prec[(add o (Proj(2, 3), Proj(3, 3))), 0]
Enter list of 2 numbers: [6, 7]
[42]
Prec> mul = Prec[add o (P 2 3, P 3 3), 0]
f: N^2 -> N^1
f = Prec[(add o (Proj(2, 3), Proj(3, 3))), 0]
Prec> exp = Prec[mul o (P 2 3, P 3 3), 1]
f: N^2 -> N^1
f = Prec[(mul o (Proj(2, 3), Proj(3, 3))), 1]
Prec> exp
f: N^2 -> N^1
f = exp
Enter list of 2 numbers: [4, 5]
[625]
```

Running from a file:

```
% cat examples/factorial.txt | ./prec
Prec> Prec> Prec> Enter list of 1 numbers: [362880]
Prec>
```

## License

[CC BY-NC](https://creativecommons.org/licenses/by-nc/4.0/legalcode)
