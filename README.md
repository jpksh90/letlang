# Expression Evaluator

The grammar of the expression evaluator:

$$
\begin{align*}
\mathit{Stmt} & := \texttt{let}~v~\texttt{=}~\mathit{Expr} \mid \mathit{Expr}  \\
\mathit{Expr} & := \textbf{lambda}~\bar{v}~\mathit{Expr} \mid \mathit{Expr}~op~\mathit{Expr} \\
              & ~~~~~~\mid \texttt{IntConst} \mid \texttt{FloatConst} \mid \texttt{C(float,float)} \mid v(\mathit{Expr}, \ldots) \\
\mathit{op} & := + \mid - \mid * \mid /
\end{align*}
$$

Simple Programs
```
let x = C(5,6); // assigns the complex number 5+6i to x
let y = C(7,9); // assigns the complex number 7+9i to y
mod(x+y) // prints the absolute of x+y
```


```
let x = 5;
let y = 7;
let z = x+y;
z+7
```