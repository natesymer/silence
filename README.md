## Felony

Lisp dialect written in Haskell. Scheme is to Felony as a scheme is to a felony - whatever that means.

Felony is not a finished language, and is far from production-ready.

### Goals

1. To be a kick ass language!
2. To learn something in making a language

### TODO

**Language**

1. More data types
	1. Thread (allows for comprehensive concurrency support)
	2. Environment (eval expr in a different environment)
2. Standard library
	1. higher order funcs
	2. convenience shit (cond, function definition syntax, etc)
3. Environment data types for passing around environments

**Parser**

1. Comments
2. Fix issues with dotted lists

### What's actually done

1. Basic parsing. Everything but dotted pairs parses.
2. Math & equality
3. Environments
4. Code loading
5. Cons cell ops
6. Misc (printing, threading, etc)

### Needs improvement

1. All math is done by first converting a number to a double. This sucks.
2. Make special forms as non-special as possible