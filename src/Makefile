ROOT=..
include $(ROOT)/Makefile.conf

PP = $(CAMLP4) -parser oq -parser op -parser pa_extend.cmo pr_dump.cmo
OCAMLINCLUDES += +camlp4 +compiler-libs
DOC_DIR = $(ROOT)/doc/html/
# Native code versions must be installed if and only if make opt was
# called previously (i.e. the files exist):
INSTALL_FILES = macro.mli pa_infix.mli delimited_overloading.mli std.mli \
  pa_do_nums.mli pa_infix.cmi pa_do.cmi pa_do_nums.cmi pa_do_top.cmi \
  pa_infix.cmo pa_do.cmo pa_do_nums.cmo pa_do_top.cma \
  $(wildcard pa_infix.cmxs pa_do.cmxs pa_do_nums.cmxs)

.PHONY: all opt byte native
all: byte
opt: native
byte: pa_do.cmo pa_do_nums.cmo pa_infix.cmo pa_do_top.cma
native: pa_do.cmxs pa_do_nums.cmxs pa_infix.cmxs

PA_DO_SOURCES = macro delimited_overloading std monad

$(PA_DO_SOURCES:=.cmo): OCAMLC_FLAGS += -for-pack Pa_do
$(PA_DO_SOURCES:=.cmx): OCAMLOPT_FLAGS += -for-pack Pa_do

pa_do.cmo: $(PA_DO_SOURCES:=.cmo)
	$(OCAMLC) -pack -o $@ $^

pa_do.cmxs: pa_do.cmx
	$(OCAMLOPT) -shared -o $@ $^

pa_do.cmx: $(PA_DO_SOURCES:=.cmx)
	$(OCAMLOPT) -pack -o $@ $^

pa_do_top.cma: pa_do_top.cmo
	$(OCAMLC) -a -o $@ $(OCAMLC_FLAGS) $^

.PHONY: doc
OCAMLDOC_FLAGS += $(if $(OCAMLINCLUDES), $(addprefix -I , $(OCAMLINCLUDES)))
doc:
	if [ ! -d "$(DOC_DIR)" ]; then mkdir -p $(DOC_DIR); fi
	$(OCAMLDOC) -html -d $(DOC_DIR) $(OCAMLDOC_FLAGS) $(wildcard *.mli)

META: META.in delimited_overloading.mli
	sed -e "s/@PACKAGE_VERSION@/$(VERSION)/" $< > $@

.PHONY: install uninstall reinstall
# TODO: copy HTML doc
install: META $(INSTALL_FILES)
	ocamlfind install $(INSTALL_FLAGS) $(PACKAGE) $^

uninstall:
	ocamlfind remove $(INSTALL_FLAGS) $(PACKAGE)

reinstall:
	$(MAKE) uninstall
	$(MAKE) install

include $(ROOT)/Makefile.ocaml

clean::
	$(RM) -f $(wildcard *~ *.omc)
