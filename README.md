# Homemade C Compiler

This C Subset Compiler is made using [Ocaml](https://ocaml.org/).\
[Menhir](http://gallium.inria.fr/~fpottier/menhir/manual.pdf),[Ocamllex](https://ocaml.org/manual/lexyacc.html) are used for front end.\
It's my final projet of our compilation class at Paris 8 University.\
Course available [here](https://pablo.rauzy.name/teaching/ic/) .

## Install
- `git clone https://github.com/Bibi210/C-Homemade-Compiler.git`

- `cd C-Homemade-Compiler`

- `rm -rf _build && dune exec Code/main.exe [Test Path] && spim load "prog.asm"`


## Explications
This projet is in 3 part Front-end,Middle-end and Back-end.
#### Front-end (Menhir , Ocamllex) :
- Lexer (We take the source code and get token out of it)
- Parser (Generate our pre code)

#### Mid-end :
- Sementics (Most of it is type verifications)
- Simplifier (Dead Code Cleanup and String Simplification)
  
#### Back-end :
- Compiler (Mips Assembly Generation)

## Tests
All tests are available in the /Tests files.\
I tried my best that commits with (... Gestion) have Valids and Verified tests.\
In old commits the README might be inaccurate

## Optimizations
- String Duplicate one time register
- Simplifier Deadcode cleanup
- Asm Cleanup of deadcode 

## Todo List
Objective is to finish, test and debug most of those. (If i can)
#### Expr :
- [X] Values
- [X] Call
- [X] Var
- [X] Assignment
- [X] Pointers
  
#### Instructions :
- [X] Declarations
- [X] Block
- [X] Do_While
- [X] While
- [X] For
- [X] If
- [X] Return
- [X] Break
- [X] Continue
- [X] Goto
- [ ] Switch
  
#### Defs :  
- [X] Functions
- [ ] Globals

#### Bonus :
- [X] Automatic Assembly comments
- [ ] Arrays
- [ ] Struct
- [X] Typedef (Works Well if we avoid mixing with pointers)


Dibassi Brahima\
L3 Informatique
