# Sqisp

## Features

### Data Types
- [x] Numbers
- [x] Strings
- [x] Keywords
- [x] Nil
- [x] Bools
- [x] Hash-maps
- [x] Hash-sets
- [x] Vectors

### Special Forms
- [x] `bis` :: Namespace for builtin sqf functions
- [x] `global` :: Intern global bar auto munging sqf varname
- [x] `if` :: If/Else expression
- [x] `do` :: Do IIFE expression syntax
- [x] `let` :: Define private variables for implicit do
- [x] `+,/,-,*,%` :: Basic math forms compiling to infix notation
- [x] `>=,>,=,<,<=` :: Compirson infix operators
- [ ] `fn` :: Define lambda expressions
- [ ] `quote` :: Yield unevaluated form
- [ ] `throw` :: Throw an exception
- [ ] `try/catch` :: Catch exceptions in calls
- [ ] `defmacro` :: Define compile time macros

### Compiler Features
- [ ] Sequence destructuring 
- [ ] Hash-map destructuring
- [ ] Compile time syntax exceptions
  - [ ] Undefined var exceptions
  - [ ] Syntax errors
- [ ] Macro expansion syntax
- [ ] Standard Library

## Macros
- [ ] `cond`
- [ ] `->`
- [ ] `->>`
- [ ] `as->`
- [ ] `defn`
- [ ] `if-not`
- [ ] `when`
- [ ] `unless`
