# Polyomino puzzle solver

https://en.wikipedia.org/wiki/Polyomino

Given a "building set" of polyominos and a universe polyomino, program outputs whether it is possible to build the universe polyomino with polyominos from the building set. 

This problem is actually just a restricted version of the exact cover problem, and as such it is solved using Knuth's Algorithm X (https://en.wikipedia.org/wiki/Knuth's_Algorithm_X).

## Requirements
A working `ghc` compiler: https://www.haskell.org/ghc/ or any online Haskell compiler.

## Usage
### Compile Main

    ghc Main.hs

### Run program
`./Main "input.txt"`
### Input
Program accepts text inputs of the polyominos as shapes built from points on the plane, in the following format:

` (u0_x,u0_y) (u1_x,u1_y) ... (un_x,un_y) `

` (pA0_x,pA0_y) (pA1_x,pA1_y) ... (pAn_x,pAn_y) `

` ... `

` (pN0_x,pN0_y) (pN1_x,pN1_y) ... (pNn_x,pNn_y) `

Where `u0`, `u1` etc. are points forming the universe polyomino and `pA0`, ..., `pAn` are points forming polyomino A from the polyomino set (and so on until the N polyomino).
#### Example input
(0,0) (0,1) (0,2) (1,0) (1,1) (1,2) (2,0) (2,1) (2,2)

(0,0) (0,1) (1,0)

(0,0) (1,0)

(1,0) (1,1) (0,1) (1,2)

Test inputs can be found in `tests`

### Output
Program outputs the graphical representation of the inputted universe polyomino, all the polyominos from building set, and a boolean answer to the puzzle.
#### Example output
Your universe:

ooo

ooo

ooo


Your polyominos:

oo

o 


oo


 o
 
oo

 o


Solution to the problem: True.
