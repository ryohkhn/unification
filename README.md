# Unification

This file is divided in two parts.
The first part describes the project that you have to
realise to get a partial note for the module 
"Programmation Fonctionelle Avancé".

## PART I: Scientific content the project

To realise this project you will implement a type inference
algorithm that works over terms of a programming language
for functional programming.


## Terms = Expressions = Programs

- [Term](./lib/term.mli) This module contains the syntax of
  the minimal programming language to we use in this project.
  Terms (i.e. programs) are values of type `Term.t`.
  This language is "applicative", i.e. fit for functional programming,
  thanks to the constructors for application (`App`) and
  for function definition (`Fun`).


## Aim of the project

The [third lecture]
(https://gaufre.informatique.univ-paris-diderot.fr/Bernardi/pfa-2324/tree/master/week03)
describes two algorithms:
the first one transforms any given program into a system of
equations, and the second one is a unification algorithm that solves
such systems.

To realise this project you will have to implement the following modules:

1. [typeSubstitution](./lib/typeSubstitution.ml)
   You must implement at least:
   - [X] `type t`, i.e. how to represent syntactic substitutions in memory,
   - [X] `val apply`,  which applies a syntactic substitution to a type
   - [X] `val compose`, which computes the substitution obtained composing two given substitutions.
    
   
1. [unification](./lib/unification.ml) 
   You must implement at least:
   - [X] `val unify` which given two type `t1` and `t2`, 
	 must compute the substitution `s` such that if
	 `unify t1 t2 = Some s` then `apply s t1 = apply s t2`.

   You can of course use the Herbrand / Robinson algorithm
   to start designing your implementation.


1. [inference](./lib/inference.ml)
   You must implement at least:
   - [X] `val typeof`, which given a term  `t` must compute either
   `None`, if there is no type for `t`, or `Some ty`, if ty is the type of term `t`.

You may add more definitions to each of these modules, and extend their signatures accordingly.
You may also create new compilation units (i.e. new `.ml` files).

1. You may, and *should*, extend the [testing module](./test/test_projet_pfa_23_24.ml) with additional
tests, or replace it with a testing framework of your choice (using e.g. QCheck).
   
## Requirements

### 1. Install OPAM

[OPAM](https://opam.ocaml.org/) is the package manager for OCaml. It
is the recommended way to install the OCaml compiler and OCaml
packages.

The following should work for macOS and Linux:
````sh
bash -c "sh <(curl -fsSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)"
````

## Build

To build the project, type:

```
$ dune build
```

For continuous build, use

```
$ dune build --watch
```

instead.

## Running your main file

To run your code you will have to implement 
`let () = ...` in the file  `main.ml`, and
then run it via

```
$ dune exec 
```

## Testing your code

To test the project, type:

```
$ dune runtest
```

This can be combined with continuous build & test, using

```
$ dune runtest --watch
```
