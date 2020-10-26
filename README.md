# otigre

A venture into compilers in ocaml. Implementing the Tiger language as described by [the paper](http://www.cs.columbia.edu/~sedwards/classes/2002/w4115/tiger.pdf).


# Implementation

otigre is written using menhir + ocamllex. Using `dune` to compile it.

To build it, run `dune build main.exe`. A file will be placed in `./_build/default/` called `main.exe`. This is the executable.



## Parse tree
![viz.svg](https://github.com/emiflake/otigre/blob/develop/viz.svg)
