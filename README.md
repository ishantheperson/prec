# prec - Primitive recursion interpreter 

## Dependencies/Compilation

No dependencies are required besides Haskell itself.
To compile, simply run `make`.

## Usage

Interactive mode:

```
% ./prec
Prec> Prec[S o P 2 3, P 1 1]
Enter list of 2 numbers: [1,2]
[3]
```

Running from a file:

```
% cat examples/factorial.txt | ./prec
Prec> Prec> Prec> Enter list of 1 numbers: [362880]
Prec>
```

## License

[CC BY-NC](https://creativecommons.org/licenses/by-nc/4.0/legalcode)
