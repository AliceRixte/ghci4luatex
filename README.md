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

* You can use `HaTeX` :

```latex

\begin{ghci}
:set -XOverloadedStrings
\end{ghci}

\begin{ghci}
import Text.LaTeX
\end{ghci}

\hask{printTex (section "A section using HaTeX")}
``` 


## Install

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
  
5. Open an other shell and compile to pdf:

```
make latex
```

## Workflow in Visual Studio Code with LaTeX workshop

1. Install the [LaTeX Workshop](https://marketplace.visualstudio.com/items?itemName=James-Yu.latex-workshop) extension.
2. In `settings.json` , add the following
```json
"latex-workshop.latex.recipes": [
        {
            "name": "ghci4luatex",
            "tools": [
                "mklatex"
            ]
        }
    ],
"latex-workshop.latex.outDir": "./build/",
"latex-workshop.latex.tools": [
        {
            "name": "mklatex",
            "command": "make",
            "args": [
                "latex",
                "lhs=%DOCFILE%"
            ],
            "env": {}
        }
    ],
```





