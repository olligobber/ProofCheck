# WFF CFG

A Context Free Grammar (CFG) for general Well Formed Formulas (WFF), implemented in `src/Parser.elm`.

## Character sets

In this document, `s` represents symbol characters, `` `~!@#$%^&*_+-=[]{}|\:;\"',<.>/? ``, and `p` represents upper and lowercase letters.

## Pre-Processing

Before parsing a WFF, all space characters, ` \t\n\r`, are removed from the string.

## Simple CFG

The following CFG produces all acceptable WFF:

```
A -> B
A -> B S B
A -> S B
B -> P
B -> ( A )
P -> p P
P -> p
S -> s S
S -> s
```

## LL(1) CFG

To parse expressions we want an LL(1) CFG. The following CFG produces all acceptable WFF and is LL(1):

```
A -> B C    1
A -> S B    2
B -> P      3
B -> ( A )  4
C -> null   5
C -> S B    2
P -> p Q    6
Q -> p Q    6
Q -> null   5
S -> s T    7
T -> s T    7
T -> null   5
```

The production rules are numbered to match their representation in code.

Below is the LL(1) lookup table for the CFG:

|     | `p` | `(` | `s` | EOF | `)` |
| :-: | :-: | :-: | :-: | :-: | :-: |
| `A` | 1   | 1   | 2   |     |     |
| `B` | 3   | 4   |     |     |     |
| `C` |     |     | 2   | 5   | 5   |
| `P` | 6   |     |     |     |     |
| `Q` | 6   |     | 5   | 5   | 5   |
| `S` |     |     | 7   |     |     |
| `T` | 5   | 5   | 7   |     |     |
