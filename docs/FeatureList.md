# Bant Feature List

## Types
### Primitive Types
* _int_
* _char_
* _string_
* _bool_
* _null_

### Collection Types  (with arbitrarily nested access)
* _List_
* _Array_
* _Tuple_
* Dictionary as _Dict_

### User-Defined Types
* Algebraic Data Types (with recursive definitions possible)
* Type-traits

## Functions
### HOF Features
* Currying
* Closures
* Partial application

### Other
* Implicit tail recursion via CPS phase
* Parametric polymorphism via function templates
* Default parameter values
* Function chaining with the bird `|>` operator
* Anonymous lambdas
* Implicitly returned function values for closures
* Built-in functions: Collections, Functional, Type conversion, Standard and File IO, Strings, Integers, Reflection

## Other Features
* Pattern matching by type and value
* Control flow
* Arithmetic operations: `+`, `-`, `*`, `/`, `%`
* Boolean operations: `<`, `>`, `<=`, `>=`, `==`, `!=`, `!`, `&&`, `||`
* Single-line comments
* Type-inference
* _char_ and _string_ escape characters
* Robust error reporting: line and column, type mismatch, interpreter stack on exception, etc.
* Optimization phase of sorts (post CPS)
* Includes for external file usage

## Project Nice-To-Have's
* Test suite
* Well-defined type system
* VSCode Syntax Highlighter
