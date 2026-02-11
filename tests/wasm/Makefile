TOP = ../..
include $(TOP)/Makefile
SRC = $(TOPSRC)/tests/wasm
VPATH = $(SRC)

WASM_TCC ?= $(TOP)/wasm32-tcc
NODE ?= node
OUTDIR = out

TESTS = ll_helpers call_indirect globals static_ptr_init
WASM_CFLAGS = -nostdlib -B$(TOP) -I$(TOPSRC)/include

all test wasm.all: $(addprefix run-,$(TESTS))
	@echo "wasm backend tests: OK"

run-%: $(OUTDIR)/%.wasm %.expect run-wasm.mjs
	@echo WasmTest $* ...
	@$(NODE) run-wasm.mjs $(OUTDIR)/$*.wasm > $(OUTDIR)/$*.out
	@diff -u $(SRC)/$*.expect $(OUTDIR)/$*.out
	@rm -f $(OUTDIR)/$*.out

$(OUTDIR)/%.wasm: %.c | $(OUTDIR)
	$(WASM_TCC) $(WASM_CFLAGS) $< -o $@

$(OUTDIR):
	mkdir -p $@

testswasm.%: run-% ;

clean:
	rm -rf $(OUTDIR)
