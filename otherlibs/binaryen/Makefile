LIBNAME=binaryen

COBJS=binaryen_stubs_exports.$(O) binaryen_stubs_functions.$(O) \
binaryen_stubs_literals.$(O) binaryen_stubs_ops.$(O) binaryen_stubs_types.$(O) \
binaryen_stubs_expressions.$(O) binaryen_stubs_globals.$(O) binaryen_stubs_memory.$(O) \
binaryen_stubs_settings.$(O) ocaml_helpers.$(O) binaryen_stubs_features.$(O) \
binaryen_stubs_imports.$(O) binaryen_stubs_modules.$(O) binaryen_stubs_tables.$(O)

CAMLOBJS=\
type.cmo expression.cmo literal.cmo global.cmo table.cmo function.cmo import.cmo op.cmo \
memory.cmo element_segment.cmo export.cmo passes.cmo settings.cmo module.cmo

LDOPTS=-lbinaryen_c

LINKOPTS=-lbinaryen_c

CHEADERS=ocaml_helpers.h binaryen-c.h

include ../Makefile.otherlibs.common

ifeq "$(UNIX_OR_WIN32)" "win32"
LN = cp
else
LN = ln -sf
endif

%.ml: binaryen_ml/src/%.ml
	$(LN) $< $@

%.mli: binaryen_ml/src/%.mli
	$(LN) $< $@

%.c: binaryen_ml/src/%.c
	$(LN) $< $@

%.h: binaryen_ml/src/%.h
	$(LN) $< $@

binaryen-c.h: binaryen/src/binaryen-c.h
	$(LN) $< $@

wasm-delegations.def: binaryen/src/wasm-delegations.def
	$(LN) $< $@

$(COBJS:.$(O)=.c): $(CHEADERS) wasm-delegations.def

binaryen/lib/libbinaryen.a:
	emcmake cmake -S binaryen -B binaryen -G "Unix Makefiles" \
		"-DCMAKE_CXX_FLAGS=-Wno-unused-variable -Wno-maybe-uninitialized" \
		-DBUILD_TESTS=OFF -DBUILD_TOOLS=OFF -DBUILD_STATIC_LIB=ON \
		-DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=binaryen
	$(MAKE) -C binaryen
libbinaryen_c.a: binaryen/lib/libbinaryen.a
	$(LN) $< $@

lib$(CLIBNAME).$(A): libbinaryen_c.a

clean::
	$(MAKE) clean -C binaryen
	rm -f *.c *.h wasm-delegations.def
	rm -f *.ml *.mli

.PHONY: depend
depend: $(CAMLOBJS:.cmo=.ml) $(CAMLOBJS:.cmo=.mli) binaryen-c.h
	$(OCAMLRUN) $(ROOTDIR)/boot/ocamlc -depend -slash *.mli *.ml > .depend

include .depend
