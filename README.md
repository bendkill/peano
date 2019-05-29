# peano

A (hopefully) type-sound interpretter for numeric and boolean computations,
evaluated with Peano arithmetic, implemented in Scala.

## Run

`peano` depends on [sbt](https://www.scala-sbt.org/download.html),
Scala's project management tool. Once you have `sbt` installed, run 
```
sbt run
``` 
to start the interpretter. It will automatically compile the project.

We recommend using `rlwrap`, which enables a Unix-like command line editor
inside the interpretter, like so:
```
rlwrap sbt run
``` 

## Usage

`peano` uses an intuitive calculator syntax for computations: computations can
be written in the usual way. For example
* `1 + 1`
* `1.5*3.7`
* `1.6 % -(2.7 + 1)`


We also provide several built-in functions for convenience. For example
* `div (4 + 4) 2` divides `4 + 4` by `2`.
* `gcd 24 16` gives the greatest common denominator.

`peano` represents terms somewhat opaquely according to the [Peano
arithmetic](https://www.wikiwand.com/en/Peano_axioms). There are four "types" in
the pure `peano` language: Bools, Naturals, Integers, and Rationals:
* A Bool is either "true" `T` or "false" `F`.
* A Natural `n` is either "zero" `Z`or the "successor" of another natural `S n`.
* An Integer consists of a Bool "is negative" and a Natural "absolute value."
* A Rational consists of an Integer "numerator" and a natural "denominator"
