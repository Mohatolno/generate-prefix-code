all :
	ocamlc -c gen_prefix.ml
	ocamlc graphics.cma -o prefix gen_prefix.cmo

clean :
	rm *.cmi *.cmo prefix

