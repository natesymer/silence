## Hisk

Lisp interpreter written in Haskell. It's also sorta it's own Lisp dialect.

### Progress

a. `read` function (to read source) **100%**
b. `eval` function **0%**

### Hisk module
<table>
<tr>
<td><code>hiskEval :: String -> String</code></td>
<td>Evaluates a line of Lisp</td>
</tr>
<tr>
<td><code>hiskRead :: String -> HiskObject</code></td>
<td>Generates a Lisp/Scheme cons cell AST from some Lisp code</td>
</tr>
<tr>
<td><code>hiskRepl :: String -> IO ()</code></td>
<td>Given a prompt, it runs a Lisp REPL</td>
</tr>
</table>