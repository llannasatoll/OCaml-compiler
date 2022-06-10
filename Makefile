SOURCES = operator.ml type.ml syntax.ml parser.mly lexer.mll \
	  gensym.ml knormal.ml env.ml typing.ml \
	  alpha.ml beta.ml eta.ml assoc.ml elim.ml constf.ml \
	  closure.ml \
	  register.ml prealloc.ml alloc.ml \
	  float.c code.ml \
	  main.ml
RESULT = compiler
OCAMLMAKEFILE = $(OPAM_SWITCH_PREFIX)/lib/ocaml-makefile/OCamlMakefile
include $(OCAMLMAKEFILE)

.SUFFIXES: .x .run

%.s : %.ml headerIntel.s compiler
	cp headerIntel.s $@
	cat $< | ./compiler >> $@

%.x : %.s mainIntel.c arrayIntel.s
	gcc -m64 mainIntel.c arrayIntel.s $< -o $@

.x.run :
	time env PATH=".:$(PATH)" $<
