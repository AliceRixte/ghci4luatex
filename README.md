# ghci4luatex : a GHCi session in LuaTeX

Run a GHCi session within a latex document :

* The `ghci` environment evaluates haskell code without printing anything :

```latex
\begin{ghci}
x :: Int
x = 4

y :: Int
y = 5
\end{ghci}
```

* The `hask` command evaluates any ghci command and prints in Haskell what GHCi printed :

```latex
The sum of $x$ and $y$ when $x = \hask{x}$ and $y = \hask{y}$ is $\hask{x + y}$.
```


## Quick start

1. Install `haskell`, `stack` and `lhs2tex` 

2. Clone this repository :

```
git clone https://github.com/AliceRixte/ghci4luatex.git
cd ghci4luatex
```

3. Run the ghci server :

```
make run
```

4. Edit `main.tex`
  
5. Open an other shell and compile to pdf :

```
make latex
```



