# Homemade C Compiler

This C Subset Compiler is made using [Ocaml](https://ocaml.org/) and [Menhir](http://gallium.inria.fr/~fpottier/menhir/manual.pdf) for front end.\
Its my final projet of our compilation class at Paris 8 University.\
Course available [here](https://pablo.rauzy.name/teaching/ic/) .

## Install
- `git clone https://github.com/Bibi210/C-Homemade-Compiler.git`

- `cd C-Homemade-Compiler`

- `rm -rf _build && dune exec Code/main.exe [Test Path] && spim `

- ```load "prog.asm"```

- ```run```

## Explications
This projet is in 3 part Front-end,Middle-end and Back-end.
#### Front-end :
- Lexer 
- Parser

#### Mid-end :
- Sementics 
- Simplifier
  
#### Back-end :
- Compiler 


## Optimizations
- String Duplicate 1 time register
- Asm Cleanup of deadcode (which is not conditional)
- Do not compile rest of block after break/continue or return

## Todo List
Objective is to finish, test and debug most of those.

#### Expr :
- [X] Values
- [X] Call
- [X] Var
- [X] Assignment
- [ ] Pointers
  
#### Instructions :
- [X] Declarations
- [X] Block
- [X] Do_While
- [X] While
- [X] For
- [X] If
- [X] Return
- [X] Break
- [ ] Continue (To opti)
- [ ] Switch
- [ ] Goto
  
#### Defs :  
- [ ] Functions
- [ ] Globals

#### Bonus :
- [ ] Arrays
- [ ] Struct
- [ ] Typedef (if I can)


Dibassi Brahima\
L3 Informatique
