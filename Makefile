
.PHONY: all opt byte native install uninstall reinstall htdoc doc test tests

all byte native opt:
	ocaml setup.ml -build

configure-release:
	ocaml setup.ml -configure --disable-tests --disable-examples

configure-test:
	ocaml setup.ml -configure --enable-tests --disable-examples

doc install uninstall reinstall test: all
	ocaml setup.ml -$@

.PHONY: clean distclean dist-clean
clean:
	-ocaml setup.ml -clean

distclean dist-clean::
	-ocaml setup.ml -distclean
