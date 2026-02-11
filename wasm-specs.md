# Restricted WebAssembly Backend Plan for TinyCC

## 1. Goal

Implement a new TinyCC backend that targets **freestanding wasm32** for numeric kernels (for example `../miniexpr`), with no WASI dependency and no host OS runtime requirements.

Target profile name in this document:

- `wasm32-freestanding` (equivalent to `wasm32-unknown-unknown`)

Primary outcome:

- Compile C kernel code to a `.wasm` module exporting selected functions.
- Support enough C to compile math-heavy kernels reliably.
- Keep scope constrained to reduce implementation risk.


## 2. Scope

### In scope (MVP)

1. `wasm32` pointers (`PTR_SIZE=4`, `LONG_SIZE=4`).
2. C functions with primitive args and returns:
   - `int`, `unsigned int`, `long` (32-bit),
   - `long long`/`unsigned long long` (64-bit),
   - `float`, `double`,
   - pointers.
3. Local variables, stack frames, scalar loads/stores.
4. Arithmetic and comparisons on integer and floating types.
5. Branching:
   - `if/else`
   - `while`, `for`, `do`
   - `switch` (initially via lowering strategy described below).
6. Direct function calls between compiled functions.
7. Static/global data (`.data`, `.bss`, rodata) mapped to linear memory.
8. Emission of valid wasm module sections:
   - Type, Function, Memory, Global, Export, Code, Data
   - optional Name section for debug.

### Out of scope (MVP)

1. WASI and OS integration.
2. `-run`, JIT relocation/execution.
3. Inline/global asm support.
4. Variadic functions (`...`, `va_start`, `va_arg`).
5. Dynamic linking, ELF shared objects, PLT/GOT logic for wasm.
6. Full C ABI compatibility for complex struct passing/return.
7. Function pointer calls (`call_indirect`) in phase 1.
8. Threading and full atomics memory model support.
9. Debug info parity (DWARF/stabs in wasm).


## 3. Target ABI and Runtime Model

### 3.1 Calling convention

Use native wasm function signatures for direct calls:

- C integral/fp scalars map directly to wasm params/results.
- Pointer types are `i32`.

Struct returns/passing (MVP):

- force memory-based behavior:
  - return structs through hidden sret pointer (no multi-register struct return).
  - pass large structs by pointer when needed.

### 3.2 Stack/frame model

Use linear-memory stack with mutable global stack pointer:

- global `__stack_pointer: i32`
- function prolog:
  - decrement SP by frame size (+ alignment)
  - keep frame base in a wasm local
- epilog:
  - restore SP

Rationale:

- matches TinyCC expectations for addressable locals and pointer arithmetic.
- avoids trying to map C stack objects directly to wasm locals.

### 3.3 Memory layout

Single wasm memory (minimum 1 page), exported as `memory`.

- Data segments initialize `.data`/rodata.
- `.bss` allocated in linear memory as zero-initialized region.
- stack region starts above static data high-water mark.


## 4. Backend Architecture

### 4.1 New backend files

1. `wasm-gen.c`
   - implements target codegen hooks declared in `tcc.h`:
     - `load`, `store`
     - `gfunc_call`, `gfunc_prolog`, `gfunc_epilog`
     - `gen_opi`, `gen_opf`, conversions
     - jumps and label patch interfaces
     - VLA helpers (initially reject or minimally support)
2. `wasm-link.c`
   - defines target constants needed by `TARGET_DEFS_ONLY` include model.
   - stubs/minimal relocation policy for wasm path.
3. `tccwasm.c`
   - module writer: takes compiled sections/symbols and emits wasm binary.

### 4.2 Control-flow lowering strategy

TinyCC internally generates unstructured labels/jumps. Wasm requires structured control flow.

Plan:

1. Emit a temporary backend IR from `wasm-gen.c`:
   - basic blocks
   - pseudo instructions
   - explicit branch edges.
2. Build CFG per function.
3. Lower CFG to structured wasm using a deterministic algorithm:
   - phase 1: reducible CFG support (`if`, loops, switch lowering).
   - explicit fallback error for unsupported irreducible patterns.
4. Serialize final wasm instructions from structured form.

This avoids fragile direct mapping from TinyCC jump patch lists to wasm branch depths.

### 4.3 Value/register model

TinyCC expects `NB_REGS`/register classes. wasm has no hardware registers.

Plan:

1. Define a virtual register bank in `wasm-gen.c`.
2. Map TinyCC “registers” to wasm locals (`local.get/set/tee`).
3. Preserve TinyCC register-class semantics (`RC_INT`, `RC_FLOAT`) minimally.
4. Keep load/store semantics consistent with addressable memory model.


## 5. Required Integration Changes (file-by-file)

### 5.1 Target selection and build

1. `configure`
   - accept `--cpu=wasm32` (and aliases).
   - emit `CONFIG_wasm32` and `TCC_TARGET_WASM32`.
2. `Makefile`
   - add `DEF-wasm32 = -DTCC_TARGET_WASM32`.
   - add `wasm32_FILES` list including new backend/writer files.
   - add cross target entry (`TCC_X += wasm32`) if desired.
3. `lib/Makefile`
   - add `OBJ-wasm32` for optional support libs (phase 2+).

### 5.2 Core target macros and includes

1. `tcc.h`
   - add `TCC_TARGET_WASM32` in target macro list/default detection.
   - include `wasm-gen.c`/`wasm-link.c` under `TARGET_DEFS_ONLY`.
   - add wasm-specific prototypes (if needed).
2. `libtcc.c`
   - include new source files in `ONE_SOURCE` dispatch.
3. `tcc.c`
   - add version string target name (`wasm32`).
4. `README`, `tcc-doc.texi`
   - mention restricted wasm backend and limits.

### 5.3 Output path wiring

1. Add new format enum in `tcc.h`:
   - `TCC_OUTPUT_FORMAT_WASM`.
2. `libtcc.c`
   - parse `-Wl,-oformat=wasm` (or target default to wasm format).
3. `tccelf.c` / output dispatcher
   - route wasm target output to `tcc_output_wasm(...)` in `tccwasm.c`.
   - keep ELF/PE/Mach-O paths untouched for other targets.

### 5.4 Frontend behavior gates for restricted backend

1. `tccgen.c`, `tcctok.h`, `include/tccdefs.h`
   - disable/reject varargs builtins on wasm backend with clear errors.
2. `tccasm.c` path
   - keep asm unsupported for wasm; emit clear diagnostic.
3. Add backend guard diagnostics for unsupported features:
   - function pointers (phase 1)
   - VLAs/alloca (phase-gated)
   - unsupported long double operations.


## 6. Language/Feature Contract for Kernel Users

Documented supported subset for kernel authors:

1. No varargs.
2. No inline asm.
3. No function pointer calls.
4. Prefer POD scalar types and pointers.
5. Struct passing/return only within supported ABI path (memory-based).
6. No reliance on process/system calls.

This contract must be encoded as compile-time diagnostics, not silent miscompiles.


## 7. Phased Implementation Plan

## Phase 0: Scaffolding and compile-time plumbing

Deliverables:

1. Target macro/build integration compiles.
2. `tcc -v` reports `wasm32`.
3. Backend stubs linked, with explicit `tcc_error("not implemented")` at codegen entry points.

Exit criteria:

- `make` succeeds for wasm target build artifacts.

## Phase 1: Minimal function codegen and module writer

Deliverables:

1. `wasm-gen.c` supports:
   - integer constants/ops
   - local loads/stores
   - function prolog/epilog
   - direct calls
   - returns
2. `tccwasm.c` emits valid module with:
   - one memory
   - one exported function
   - code section and minimal data section.

Exit criteria:

- compile/run simple kernels in a wasm runtime:
  - add
  - mul
  - looped sum.

## Phase 2: Branching and CFG lowering

Deliverables:

1. Basic-block IR.
2. CFG builder.
3. Structured lowering for:
   - `if/else`
   - loops
   - `switch` lowering.

Exit criteria:

- control-flow kernel tests pass against native reference results.

## Phase 3: Floating point and conversions

Deliverables:

1. `gen_opf`, `gen_cvt_itof`, `gen_cvt_ftoi`, `gen_cvt_ftof`.
2. Correct comparisons and boolean lowering.
3. Deterministic NaN/inf behavior aligned with wasm semantics.

Exit criteria:

- fp-heavy math kernel tests pass within tolerance.

## Phase 4: Globals/data and exports

Deliverables:

1. Data/BSS placement and initialization.
2. Symbol export policy:
   - export all non-static functions by default for MVP, or
   - export selected symbols via attribute/option.
3. Pointer relocation/fixups for internal data references.

Exit criteria:

- kernels using static lookup tables compile and run.

## Phase 5: Hardening, diagnostics, and docs

Deliverables:

1. Clear diagnostics for every unsupported construct.
2. Test suite additions (backend + runtime validation).
3. Documentation of guarantees and non-goals.

Exit criteria:

- stable compile/run behavior for target kernel corpus.

## Status Snapshot

Implemented in this branch:

1. `wasm32` target plumbing across `configure`, `Makefile`, `tcc.h`, `libtcc.c`, `tcc.c`, `tccelf.c`.
2. Working restricted backend IR in `wasm-gen.c` for:
   - scalar integer/floating loads/stores and arithmetic,
   - split-register `long long` lowering bridged to wasm `i64` (calls/returns, `TOK_ADDC*`/`TOK_SUBC*`/`TOK_UMULL`, int<->fp conversions),
   - helper/libcall compatibility by lowering common `__*di3` helper calls in wasm emission (`__divdi3`, `__udivdi3`, `__moddi3`, `__umoddi3`, `__ash*l*di3`, `__floatundi*`, `__fixuns*di`),
   - comparisons and boolean lowering,
   - prolog/epilog, direct calls, indirect calls (`call_indirect`), returns,
   - control flow via jump lists.
3. Working wasm module writer in `tccwasm.c`:
   - type/function/table/memory/global/export/element/code/data sections,
   - linear-memory stack (`__stack_pointer` global),
   - static data/rodata segments and bss placement,
   - data/rodata relocation application for pointer initializers,
   - stable symbol resolution for IR address ops via captured symbol index/token metadata (avoids stale `Sym*` dereference issues).
4. Control-flow lowering strategy implemented as PC-dispatch in wasm code, enabling unstructured TinyCC jumps (`if/loops/switch/goto` patterns) without CFG structuring pass.
5. Compile-time diagnostics for unsupported features:
   - variadics,
   - computed goto,
   - VLA/alloca.

Verified with runtime execution (Node wasm instantiation) on:

1. Straight-line integer kernels (`add`, `muladd`).
2. Loop/switch kernels.
3. Floating-point kernels (`float` and `double`).
4. Direct internal function calls.
5. Global data/rodata/bss access.
6. Dedicated wasm regression suite in `tests/wasm`:
   - `ll_helpers` (i64 helper/libcall lowering),
   - `call_indirect` (function pointer indirect calls, including i64/f64 signatures),
   - `globals` (rodata/data/bss initialization and mutation),
   - `static_ptr_init` (static pointer initializers to internal rodata/functions),
   wired into `tests/Makefile` as conditional `wasm-dir` when `wasm32-tcc` and `node` are available.

Not yet implemented:

1. WASI/host import model.
2. Dedicated wasm debug info emission.


## 8. Test Strategy

### 8.1 Unit-level backend tests

1. Instruction encoding helpers in `tccwasm.c`.
2. CFG to structured wasm lowering on synthetic control-flow graphs.
3. Type/signature mapping tests.

### 8.2 Compiler integration tests

Add wasm backend tests under `tests/`:

1. Integer arithmetic kernels.
2. Loop and branch kernels.
3. Float kernels.
4. Pointer indexing kernels.
5. Unsupported-feature diagnostics tests.

### 8.3 Runtime validation

1. Run generated `.wasm` in `wasmtime` (or equivalent) in CI.
2. Compare outputs with native `tcc`/`clang` reference.


## 9. Risk Register and Mitigation

1. **Control-flow lowering complexity**
   - Mitigation: explicit CFG IR and staged reducible CFG support.
2. **Mismatch with TinyCC register/value assumptions**
   - Mitigation: virtual register local mapping and aggressive assertions.
3. **Silent ABI misbehavior on structs**
   - Mitigation: force memory-based struct ABI in MVP, add explicit checks.
4. **Feature creep**
   - Mitigation: lock MVP contract to kernel-only subset.
5. **Runtime helper dependencies (libtcc1)**
   - Mitigation: start with helper-free subset, then add selected helpers.


## 10. Milestones and Acceptance Criteria

### M1: Build integration complete

- `wasm32` target selectable and compiler builds.

### M2: Straight-line kernels work

- math kernels without branches compile to valid wasm and execute correctly.

### M3: Branch-heavy kernels work

- `if/loop/switch` kernel set passes.

### M4: Float kernel parity

- fp kernels match expected numerical behavior.

### M5: Miniexpr kernel pass

- selected `../miniexpr` kernels compile and run as wasm module exports.


## 11. Suggested Initial Task Queue

1. Add target plumbing (`configure`, `Makefile`, `tcc.h`, `libtcc.c`, `tcc.c`).
2. Create backend skeleton files (`wasm-gen.c`, `wasm-link.c`, `tccwasm.c`).
3. Add wasm output dispatch.
4. Implement function/module minimal happy path.
5. Add first runtime test harness for generated wasm.
6. Iterate on control flow and float support.


## 12. Definition of Done (restricted backend)

Backend is considered done for this project scope when:

1. Kernel subset compiles to `.wasm` reliably.
2. Generated modules run in a standard wasm runtime without WASI.
3. Unsupported language features fail fast with precise diagnostics.
4. Added wasm tests are stable in CI.
5. Documentation states exact supported subset and known gaps.
