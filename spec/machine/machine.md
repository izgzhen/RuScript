# Abstract Machine

## Convention
We use $\#$ to destruct an abstract symbol into product tuple as defined.

In abstract definition, $[p]$ means a vector of $p$; $[k \mapsto v]$ means a partial mapping from $k$ to $v$. Mapping is always represented by $m$

In concrete rules, $[p]$ means a vector of $p$, $m[k \mapsto v]$ means update mapping $m$ with $v$ indexed by $k$, $[p]@i$ means the $i$th element of $[p]$ vector. $m@k$ means the element of $m$ indexed by $k$

$p?$ means an `Option` type in abstract definition, we use $p$ to match the `Some` case, and $\texttt{None}$ to match a `None` case.

NOTE: This model is only to provide an intuition for formalization. It might not reflect the implementation as it is.

## Abstract Object
Class: $Cls \stackrel{def}{\equiv} (i?, [s \mapsto f], [s])$

Function: $f \stackrel{def}{\equiv} ([I], i)$

Instance Object: $ist \stackrel{def}{\equiv} (i, [x])$

Any object: $x, top, next ...$

Primitives:

* string: $s$
* boolean: $b = \{ \textbf{tt},\textbf{ff}\}$
* integer: $i$
* nil: $\{\textbf{nil}\}$

Other symbols:

* Local variables: $l \stackrel{def}{\equiv} [x]$
* Environment: $E \stackrel{def}{\equiv} ([Cls], [f])$
* Program Counter: $c \stackrel{def}{\equiv} i$
* Stack: $S$, supporting list pattern to express pop and push, for example, $x:y:S$ means $S$ is at least 2 elements long, and top element is $x$, next element is $y$
* Implicit Side-effects: $\epsilon$

## Local state transition

Transition: $$E \vDash \langle I, (l, c,S) \rangle \triangleright (l', c', S')$$

Rewriting rules:


$$\begin{aligned}
E &\vDash \langle \texttt{JUMP}(i), (l, c,S) \rangle &\triangleright & \quad (l, c + i, S) \\
E &\vDash \langle \texttt{JUMPT}(i), (l, c, \mathbf{tt}:S) \rangle &\triangleright & \quad (l, c + i, S) \\
E &\vDash \langle \texttt{JUMPT}(i), (l, c, \mathbf{ff}:S) \rangle &\triangleright & \quad (l, c, S) \\
E &\vDash \langle \texttt{JUMPF}(i), (l, c, \mathbf{ff}:S) \rangle &\triangleright & \quad (l, c + i, S) \\
E &\vDash \langle \texttt{JUMPF}(i), (l, c, \mathbf{tt}:S) \rangle &\triangleright & \quad (l, c, S) \\
E &\vDash \langle \texttt{PUSH}(i), (l, c,S) \rangle &\triangleright & \quad (l, c, l[i]:S) \quad \text{if len(l) > i}\\
E &\vDash \langle \texttt{POP}(i), (l, c, top:S) \rangle &\triangleright & \quad (l[i \mapsto top], c, S) \quad \text{if len(l) > i}\\
E &\vDash \langle \texttt{NEW}(i), (l, c,S) \rangle &\triangleright & \quad (l, c, \texttt{new}(E, i, S):S) \\
E &\vDash \langle \texttt{PUSHA}(s), (l, c, top:S) \rangle &\triangleright & \quad (l, c, \texttt{get}(top,s):S) \\
E &\vDash \langle \texttt{POPA}(s), (l, c, top:next:S) \rangle &\triangleright & \quad (l, c, \texttt{set}(\texttt{copy}(next), s, top, E):S) \\
E &\vDash \langle \texttt{PUSHSTR}(s), (l, c,S) \rangle &\triangleright & \quad (l, c, s:S) \\
E &\vDash \langle \texttt{PUSHINT}(i), (l, c,S) \rangle &\triangleright & \quad (l, c, i:S) \\
E &\vDash \langle \texttt{PUSHBOOL}(i), (l, c,S) \rangle &\triangleright & \quad (l, c, \mathbf{ff}:S)  \quad \text{if i = 0}\\
E &\vDash \langle \texttt{PUSHBOOL}(i), (l, c,S) \rangle &\triangleright & \quad (l, c, \mathbf{tt}:S)  \quad \text{if i <> 0}\\
E &\vDash \langle \texttt{PUSHLIST}, (l, c,S) \rangle &\triangleright & \quad (l, c, []:S) \\
E &\vDash \langle \texttt{PUSHNIL}, (l, c,S) \rangle &\triangleright & \quad (l, c, \mathbf{nil}:S) \\
\end{aligned}$$

## Frame state transition (single-step)

Transition: $$E \vDash \langle I, (l, c,S) \rangle \rightarrow (l', c', S'), \epsilon$$

Rewriting Rules:


$$
\frac{\texttt{run}(E, [f]@i, S) \leadsto S', \epsilon}
     {E\#([Cls], [f]) \vDash \langle \texttt{CALL}(i), (l, c,S) \rangle \rightarrow (l, c + 1, S'), \epsilon }
$$$$
\frac{\texttt{run}(E, \texttt{method}(E, s, ist), ist:S) \leadsto S', \epsilon}
     {E \vDash \langle \texttt{INVOKE(s)}, (l, c, ist:S) \rangle \rightarrow  (l, c + 1, S'), \epsilon}\\
$$$$
\frac{\texttt{run}(E, \texttt{method}(E, s, ist), ist:S) \leadsto S', \epsilon}
     {E \vDash \langle \texttt{INVOKE(s)}, (l, c, obj:S) \rangle \rightarrow (l, c + 1, S), \epsilon}\\
$$$$
\frac{E \vDash \langle I, (l, c, S) \rangle \triangleright (l', c', S')}
      {E \vDash \langle I, (l, c, S) \rangle \rightarrow  (l', c' + 1, S'), \emptyset}
$$


## Frame state transition (multi-step)

Transition: $$E, C \vDash (l, c, S) \rightarrow^* S', \epsilon$$

Rules:
$$\vDash \langle \texttt{RET}, (l, c, S) \rangle \rightarrow^* S, \emptyset $$

$$\frac{E, C \vDash \langle C[c], (l, c,S) \rangle \rightarrow (l', c', S'), \epsilon \quad
        E, C \vDash (l', c', S') \rightarrow^* S'', \epsilon'}
       {E, C \vDash (l, c, S) \rightarrow^* S'', \epsilon \cup \epsilon'}$$


## Misc

$$\texttt{run}(E, (C, i), S) \leadsto S', \epsilon =
  E, C \vDash (l, 0, S) \rightarrow^* S', \epsilon \quad \text{where } l = i * [0]$$

$$\texttt{new}(([Cls], [f]), i, S) = \texttt{InstanceObj}(k, i, S) \quad k = \texttt{len}(\texttt{attrs}([Cls]@i)$$

$$\texttt{method}(([Cls], \_), s, ist\#(i, \_)) = m@s \quad \text{where } ([Cls]@i)\#(\_, m, \_)$$
