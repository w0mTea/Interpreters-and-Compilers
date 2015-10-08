This is a simple interpreter for untyped lambda calculus.

In this implementation, the character '\' is used to represent the symbol lambda.

To use the interpreter, you just need to compile it by

    ghc Lambda.hs

and then run the binary file created by ghc.

You need to give file paths to run the interpreter and at least one path is needed.

An simple example:

```
cat test
> (\x.x) (\s.\z.s (s z))

./Lambda test
> (λs.(λz.(s (s z))))
```
