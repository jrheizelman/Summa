# Summa

Summa is a type-inferred, statically typed language with a syntax resembling
the C family of languages. Still very much a work in progress, the current aim
is to add the following base functionality to the compiler:
* Compilation to Java
* Type-inference for all monotypes
* Inference and static checking for polytypes within function parameters and
arguments, i.e. sum(a, b) that accepts both doubles and ints
* Static-checking of all types, with warnings for ambiguity
* Optional explicit typing for any function returns types and parameters
* Functions as first-class objects, resolvable to their return type as a result of typed parameters
* User-defined objects with class methods, and inferred polymorphic class groups
* All if, else, for, while, etc essential statement blocks