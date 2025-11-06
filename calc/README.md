# Argham

**Argham** (ارقام, “numbers”) is a symbolic calculator written in **Fortran**, based on **recursive descent parsing**.
The project implements a parser, an Abstract Syntax Tree (AST) generator, and an expression evaluator with variable support.

### Features

* Arithmetic expression parser with operator precedence
* AST construction and traversal
* Numerical evaluation with a symbol table
* Modular Fortran implementation (`parser`, `ast`, `evaluator`, `symtab`)

### Future Features

* Function support

### Grammar

**Non-terminals**

```
E: Expression
P: Primary
B: Binary Operator
U: Unary Operator
T: Term
F: Factor
```

**Terminals**

```
"(", ")", "+", "-"
```

**Loops**

```
{...}
```

### References

* *Parsing Expressions by Recursive Descent* — [Theo de Raadt](https://www.engr.mun.ca/~theo/Misc/exp_parsing.htm)
* *Recursive Descent Parser* — [GeeksforGeeks](https://www.geeksforgeeks.org/compiler-design/recursive-descent-parser/)
* *Language Production* — [CORE](https://core.ac.uk/download/pdf/210661528.pdf)
* *Production (Computer Science)* — [Wikipedia](https://en.wikipedia.org/wiki/Production_%28computer_science%29)
* *Parsing* — [University of Rochester](https://www.cs.rochester.edu/users/faculty/nelson/courses/csc_173/grammars/parsing.html)
* *Abstract Syntax Tree for Programming Language Understanding and Representation* — [arXiv](https://arxiv.org/pdf/2312.00413)
