# OCaml-compiler

## 使い方

`cp headerIntel.s <file.ml>`

`cat <file.ml> | ./compiler >> <file.s>` or `make <gcd.s>`

`gcc -m64 mainIntel.c <file.s> -o <file.x>` or `make <file.x>`

`./<file.x>` or `make <file.run>`
