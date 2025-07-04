# An example for ghci4luatex

## Compiling `main.tex`

1. Run `ghci4luatex` using stack :

```
ghci4luatex --command "stack ghci"
```

2. Then, simply run `make main=main` to compile `main.tex`.

## Compiling `advanced.tex`


1. To compile this example, you need `lhs2tex`. You can install it via

```
cabal install lhs2tex
```

or

```
stack install lhs2tex
```

2. `ghci4luatex --command "stack ghci"`

3. `make main=advanced`
