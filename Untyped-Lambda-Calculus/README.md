# Setup

## Dependencies

This is a simple interpreter for untyped lambda calculus.

To install/run this interpreter, you have to install Haskell first.
I suggest you installing **haskell platform**
because this will install ghc along with some useful packages
and you don't need to install too much packages manually.

After haskell is installed, you also need a package called **Parsec**.
Just type

```
cabal install parsec
```

in a terminal and then parsec will be automatically installed.

## Compile and Run

You can run the interpreter in two ways:

1. Compile it and run the binary file
2. Run it directly without compiling

To compile it, do as the following example:

```
ghc Lambda.hs # this will create a binary file Lambda
./Lambda xxx # xxx is source file to be interpreted
```

To run it directly:

```
runhaskell Lamda.hs xxx # xxx is source file to be interpreted
```

The interpreter can access multiple files at one time.
It will interpret each file and show results.

# Syntax

In this implementation, the character '\' is used to represent the symbol lambda.

Abstract syntax:

```
Term ::= ident        # Variable
       | \ident.Term  # Abstraction
       | Term Term    # Application
```

ident means identifier, and it's a string which can be accepted by the following regular expression:

```
[a-zA-Z_][a-zA-Z0-9_]*'*
```

Some valid identifiers:

- x
- x0
- x0123
- x'

Some simple examples of lambda calculus source code:

```
(\x.x)
(\x.\y.x)
(\s.\z.s z)
(\s.\z.s (s z))
(\m.\n.\s.\z.m s (n s z)) (\s.\z.s z) (\s.\z.s (s z))
```

# Evaluating order

I've implemented two evaluating orders, **call by name** and **normal order**.

## Call By Name

By default, the interpreter uses call by name.
Consider lambda calculus bolow:

```
t1 t2
```

Under CBV(the shorthand of call by name),
t1 is first evaluated until it becomes a abstraction.
Then t2 is evaluated until it also becomes a abstraction.
Finally t1 t2 will be evaluated.

Only terms out of a abstraction will be evaluated,
and those in the abstraction will not.

Some times, CBV is not very convenient.
Consider the following code piece which increment a church number:

```
(\m.\s.\z.s (m s z)) (\s.\z.s z)
```

The final result is:

```
\s.\z.s ((\s1.\z1.s1 z1) s z)
```

## Normal Order

Under normal order, terms in a abstraction will also be evaluated,
so the example above can be evaluated to

```
\s.\z.s (s z)
```

It's easier to read, isn't it?

To enable normal order, just add "--normal-order" while running the interpreter:

```
runhaskell Lambda.hs filename --normal-order
```
