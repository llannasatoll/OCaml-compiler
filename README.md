# OCaml-compiler

## 使い方（例）

`cp headerIntel.s gcd.ml`

`cat gdc.ml | ./compiler >> gcd.s` or `make gcd.s`

`gcc -m64 mainIntel.c gcd.s -o gcd.x` or `make gcd.x`

`./gcd.x` or `make gcd.run`
