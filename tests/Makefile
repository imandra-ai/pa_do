ROOT=..
include $(ROOT)/Makefile.conf
SYNTAX_DIR = ../src
OCAMLINCLUDES += $(SYNTAX_DIR) +camlp4 +compiler-libs
PP = $(CAMLP4) -parser oq -printer a
LIBS_CMA = unix.cma $(if $(DYNLINK_SUPPORTED), dynlink.cma) \
           camlp4lib.cma str.cma
LIBS_CMXA = $(LIBS_CMA:.cma=.cmxa)

TESTS = op_basic.ml op_mix.ml op_concrete.ml \
	do_std.ml do_concrete.ml do_api.ml \
	test_macro.ml test_complex.ml

.PHONY: all byte opt native run_tests
all: run_tests
opt: native
byte: test.cma $(TESTS:.ml=.exe)
native: test.cmxa $(TESTS:.ml=.com)

$(TESTS:.ml=.exe): test.cma
$(TESTS:.ml=.com): test.cmxa
test.cma: test.cmo
test.cmxa: test.cmx

run_tests:
	$(if $(OCAML_NATIVE), $(MAKE) run_tests_native, $(MAKE) run_tests_byte)
run_tests_byte: byte
	@echo "*** Running the tests (byte)..."
	@for f in $(TESTS:.ml=.exe); do \
	  ./$$f -I $(SYNTAX_DIR) --dynlink $(DYNLINK_SUPPORTED); \
	done
run_tests_native: native
	@echo "*** Running the tests (native)..."
	@for f in $(TESTS:.ml=.com); do \
	  ./$$f -I $(SYNTAX_DIR) --dynlink $(DYNLINK_SUPPORTED); \
	done


include $(ROOT)/Makefile.ocaml

# Native Dynlink is supported since OCaml 3.11.0:
OCAML_VERSION_SPLIT = $(subst ., ,$(OCAML_VERSION))
OCAML_MAJOR = $(word 1,$(OCAML_VERSION_SPLIT))
OCAML_MINOR = $(word 2,$(OCAML_VERSION_SPLIT))
DYNLINK_SUPPORTED = $(shell \
  if [ $(OCAML_MAJOR) -ge 4 ] || \
	[ $(OCAML_MAJOR) -ge 3 -a $(OCAML_MINOR) -ge 11 ]; then echo true; fi)

clean::
	$(RM) $(wildcard *.exe *.com)
