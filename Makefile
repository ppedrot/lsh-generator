OCAMLJS=js_of_ocaml
OCAMLC=ocamlfind ocamlc -package js_of_ocaml -package js_of_ocaml.syntax -syntax camlp4o

PROG=generator

all: $(NAME).js

$(NAME).js: $(PROG).byte
	$(OCAMLJS) $(PROG).byte $(OPTIONS)

$(PROG).byte: $(PROG).ml
	$(OCAMLC) -linkpkg -o $@ $^

clean::
	rm -f *.cm[io] $(PROG).byte $(PROG).js
