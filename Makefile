PKG_NAME    = $(shell oasis query name)
PKG_VERSION = $(shell oasis query version)
PKG_TARBALL = $(PKG_NAME)-$(PKG_VERSION).tar.gz

WEB = shell.forge.ocamlcore.org:/home/groups/pa-do/htdocs

DISTFILES   = $(shell git ls-files) \
  AUTHORS.txt INSTALL.txt README.md LICENSE.txt _tags \
  $(filter-out %~, $(wildcard src/* test/* doc/* examples/*))

SCP = scp -C -p

.PHONY: all opt byte native install uninstall reinstall htdoc doc test tests

all byte native opt: configure
	ocaml setup.ml -build

configure: setup.ml
	ocaml $< -configure --enable-tests

setup.ml: _oasis
	oasis setup -setup-update dynamic

htdoc: doc
doc install uninstall reinstall test: all
	ocaml setup.ml -$@

upload-doc: doc
	$(SCP) -r _build/src/API.docdir/ $(WEB)
	cd doc/web && $(SCP) *.html *.css *.png $(WEB)

# Assume the environment variable $GODI_LOCALBASE is set
.PHONY: godi
godi: pa_do.godiva
	godiva $<

# "Force" a tag to be defined for each released tarball
.PHONY: dist tar
dist tar: $(DISTFILES)
	mkdir $(PKG_NAME)-$(PKG_VERSION)
	cp -r --parents $(DISTFILES) $(PKG_NAME)-$(PKG_VERSION)/
# Generate a compilation files not depending on oasis:
	cd $(PKG_NAME)-$(PKG_VERSION) && oasis setup
	tar -zcvf $(PKG_TARBALL) $(PKG_NAME)-$(PKG_VERSION)
	rm -rf $(PKG_NAME)-$(PKG_VERSION)

.PHONY: clean distclean dist-clean
clean:
	-ocaml setup.ml -clean
	$(RM) $(PKG_TARBALL)
	$(RM) $(wildcard $(addprefix src/*.,cmo cma cmx cmxs))
	$(RM) pa_do.godiva setup.data

distclean dist-clean::
	-ocaml setup.ml -distclean
	$(RM) $(wildcard *.ba[0-9] *.bak *~ *.odocl)
