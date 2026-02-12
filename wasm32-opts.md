# wasm32 Code Generation Performance Optimizations

## Problem Statement

TCC's wasm32 backend generates correct code but is **slower than the DSL
interpreter** for compute-heavy kernels.  On the mandelbrot benchmark
(1200×800, max_iter=200):

| Backend | Eval (ms) | Speedup |
|---------|-----------|---------|
| Interpreter | 3742 | baseline |
| TCC wasm32 JIT cold | 9011 | 0.4× |
| TCC wasm32 JIT warm | 4555 | 0.8× |
| TCC native (arm64) | 453 | **5.4×** |
| CC native (clang -O2) | 217 | **11.2×** |

The native TCC JIT (arm64/x86) achieves 5–10× speedups because it emits
straight-line machine code.  The wasm32 backend's switch-loop dispatch model
introduces ~40–60% overhead on loop-heavy kernels.

---

## Root Cause: Switch-Loop Dispatch

Every IR op is emitted as a case in a `loop` + `br_table` dispatcher:

```wasm
loop $dispatch
  block $halt
    block $case_N
      block $case_N-1
        ...
        block $case_0
          local.get $pc
          br_table $case_0 $case_1 ... $case_N $halt
        end  ;; case 0
        ;; <actual work: e.g. i32.add>
        i32.const 1          ;; set $pc = next op
        local.set $pc
        br $dispatch         ;; back to top
      end  ;; case 1
      ...
    end
  end
end
```

**Cost per IR op (non-branch):**
- 3 overhead instructions: `i32.const` + `local.set $pc` + `br $dispatch`
- 6–10 bytes of wasm bytecode
- Full `br_table` re-evaluation at loop top (O(1) but with pipeline stalls)

For a mandelbrot inner loop with ~20 ops × 200 iterations per pixel ×
960K pixels, that's ~3.8 billion redundant dispatch round-trips.

The interpreter avoids this by processing entire arrays through tight
SIMD-vectorized loops (auto-vectorized with `#pragma GCC ivdep`),
amortizing dispatch over thousands of elements.

**Why TCC uses this model:** WebAssembly's structured control flow
(no arbitrary `goto`) makes it impossible to directly translate TCC's
internal IR, which uses a flat PC-indexed op array with arbitrary jumps.
The switch-loop is the standard workaround (used by all C-to-wasm
compilers that don't do a full CFG restructuring pass).

---

## Current Code Structure

Files: `tccwasm.c` (1376 lines), `wasm-gen.c` (1063 lines),
`wasm-backend.h` (125 lines).

### IR representation (wasm-backend.h)
- **WasmOpKind**: 45 op kinds (NOP, constants, loads/stores, arithmetic,
  comparisons, jumps, conversions, calls, ret)
- **WasmOp**: per-op struct with `kind`, `type`, `r0-r2` (virtual registers),
  `imm`, `target_pc` (jump target), call metadata
- **WasmFuncIR**: array of WasmOps, `nb_ops` count, signature, frame info

### Code generation (wasm-gen.c)
- `gen_opi()`, `gen_opf()`: emit arithmetic/logic ops (1 WasmOp per C op)
- `gjmp()`, `gjmp_cond()`: emit WASM_OP_JMP / WASM_OP_JMP_CMP with
  deferred target resolution via patch chains
- `gsym_addr()`: resolves forward jumps by setting `target_pc`
- **No optimization passes** — IR is emitted directly during semantic
  analysis and output as-is

### Emission (tccwasm.c)
- `wasm_emit_function_body()`: creates the loop+br_table scaffold,
  iterates ops backwards emitting nested blocks
- `wasm_emit_case()`: 48-case switch; control flow ops (JMP, JMP_CMP,
  RET) return early; all others fall through to the 3-instruction
  dispatch tail at lines 988–990

### Key observation
Jump ops (JMP, JMP_CMP) store a `target_pc` that maps to an op index.
These targets define **basic block leaders** — the natural boundaries
for any block coalescing optimization.

---

## Option 1: Basic Block Coalescing (Recommended First Step)

### Idea

Merge consecutive non-branching ops into a single br_table case.
Instead of dispatching after every `i32.add`, accumulate straight-line
code and only dispatch at branch points (jump targets, jumps, returns).

**Before (current — 4 dispatches for 4 ops):**
```
case 5: i32.load ...    → set_pc(6), br dispatch
case 6: i32.load ...    → set_pc(7), br dispatch
case 7: i32.add         → set_pc(8), br dispatch
case 8: i32.store ...   → set_pc(9), br dispatch
```

**After (coalesced — 1 dispatch for 4 ops):**
```
case 5: i32.load ...
        i32.load ...
        i32.add
        i32.store ...  → set_pc(9), br dispatch
```

### Algorithm

1. **Identify basic block leaders** by scanning the IR:
   - Op 0 is always a leader
   - Any op whose index is a `target_pc` of a JMP/JMP_CMP is a leader
   - Any op immediately after a JMP/JMP_CMP/RET is a leader
2. **Build a leader set** (bitmap or sorted array)
3. **Modify `wasm_emit_function_body`:**
   - Allocate one block per basic block (not per op)
   - `br_table` has one entry per basic block (not per op)
   - Emit all ops in a block sequentially without intermediate dispatches
   - Only emit the dispatch tail (`set_pc` + `br`) at the end of each block
4. **Update jump target mapping:** `target_pc` → block index (not op index)

### Expected Impact

For the mandelbrot inner loop (~20 arithmetic ops between branches),
this reduces dispatches from ~20 per iteration to ~3–4 (one per branch
point).  Expected speedup: **3–5× over current wasm32 JIT**, bringing
it to **1.5–3× over interpreter** (comparable to TCC native's 5.4×,
accounting for wasm overhead).

### Complexity

- ~200–400 lines of changes in `tccwasm.c`
- No changes to `wasm-gen.c` (IR generation unchanged)
- No changes to `wasm-backend.h` (struct unchanged)
- Risk: moderate — must correctly handle the reduced br_table size and
  remapped block indices for JMP/JMP_CMP targets

### Implementation Sketch

```c
// In tcc_output_wasm or wasm_emit_function_body:

// 1. Build leader bitmap
int *is_leader = tcc_mallocz(f->nb_ops * sizeof(int));
is_leader[0] = 1;
for (int i = 0; i < f->nb_ops; i++) {
    WasmOp *op = &f->ops[i];
    if (op->kind == WASM_OP_JMP || op->kind == WASM_OP_JMP_CMP) {
        int target = wasm_pc_to_index(f, op->target_pc);
        if (target >= 0 && target < f->nb_ops)
            is_leader[target] = 1;
        if (i + 1 < f->nb_ops)
            is_leader[i + 1] = 1;
    } else if (op->kind == WASM_OP_RET) {
        if (i + 1 < f->nb_ops)
            is_leader[i + 1] = 1;
    }
}

// 2. Assign block indices
int *op_to_block = tcc_mallocz(f->nb_ops * sizeof(int));
int nb_blocks = 0;
for (int i = 0; i < f->nb_ops; i++) {
    if (is_leader[i]) nb_blocks++;
    op_to_block[i] = nb_blocks - 1;
}

// 3. Emit nb_blocks blocks instead of nb_ops blocks
// 4. In each block, emit all ops from leader to next leader
// 5. Only emit dispatch tail after the last op in each block
// 6. JMP/JMP_CMP targets: use op_to_block[target_index]
```

---

## Option 2: Structured Control Flow Reconstruction (Major Rewrite)

### Idea

Replace the switch-loop entirely.  Analyze the IR's control flow graph
and emit proper wasm structured control flow (`block`, `loop`, `if/else`,
`br_if`) that directly mirrors the C source's control flow.

**Before (switch-loop for a simple `for` loop):**
```wasm
loop $dispatch
  br_table $pc → [init, cond, body_0, body_1, ..., incr, exit]
  ;; each case dispatches back
end
```

**After (structured):**
```wasm
;; init
i32.const 0
local.set $i
block $exit
  loop $loop
    ;; condition
    local.get $i
    local.get $n
    i32.ge_s
    br_if $exit
    ;; body (straight-line)
    ...
    ;; increment
    local.get $i
    i32.const 1
    i32.add
    local.set $i
    br $loop
  end
end
```

### Algorithm (Relooper / Stackifier)

This requires a **control flow restructuring algorithm**.  Two well-known
approaches:

1. **Relooper** (Emscripten's original algorithm, Alon Zakai 2011):
   - Build CFG from IR ops
   - Recursively identify "simple" (single-entry) and "loop" shapes
   - Emit nested block/loop/if structures
   - Falls back to a label variable dispatch for irreducible graphs
   - Reference: binaryen's `RelooperReconstructor`

2. **Stackifier** (LLVM's approach, used in wasm backend):
   - Process blocks in reverse postorder
   - Insert `block` and `loop` headers at dominance boundaries
   - Convert branches to `br`/`br_if` targeting enclosing blocks
   - Simpler than relooper for reducible CFGs (which all C programs produce)

### Expected Impact

Eliminates dispatch overhead entirely.  The generated wasm would be
equivalent to what LLVM -O0 produces.  Expected: **5–8× over interpreter**
(close to native TCC, minus wasm's general overhead vs native code).

### Complexity

- **~1500–2500 lines** of new code (CFG builder + restructuring pass)
- Major changes to `tccwasm.c` (`wasm_emit_function_body` rewritten)
- Moderate changes to `wasm-gen.c` (need CFG metadata)
- New file likely needed: `wasm-cfg.c` or similar
- Risk: high — must handle all control flow patterns (nested loops,
  break/continue, switch, goto, short-circuit &&/||)
- Testing: extensive — any CFG restructuring bug produces silent
  wrong code

### When to pursue

Only after Option 1 proves insufficient and there's a clear need for
near-native wasm performance.  Option 1 captures most of the low-hanging
fruit.

---

## Option 3: Peephole Optimizations (Complements Option 1)

Small targeted optimizations that can be applied independently:

### 3a. Redundant local.get/local.set elimination

Currently each op loads registers from locals and stores results back.
Adjacent ops that produce→consume the same virtual register could use
the wasm value stack directly.

```wasm
;; Before:
local.get $r0      ;; load a
local.get $r1      ;; load b
i32.add
local.set $r0      ;; store result
local.get $r0      ;; reload for next op  ← redundant
i32.const 2
i32.mul
local.set $r0

;; After:
local.get $r0
local.get $r1
i32.add            ;; result stays on stack
i32.const 2
i32.mul
local.set $r0
```

**Impact:** ~10–20% improvement.  Reduces instruction count by eliminating
load/store pairs between consecutive ops that chain registers.

**Complexity:** ~100–200 lines.  Requires tracking which ops produce values
consumed by the immediately-next op.  Can be done during emission in
`wasm_emit_case` by peeking at the next op.

### 3b. Constant folding in IR

Evaluate `i32.const + i32.const → i32.const` and similar at IR level
before emission.  TCC already does some constant folding in `tccgen.c`,
but missed cases reach the wasm IR.

**Impact:** minor (~5%).  **Complexity:** ~50–100 lines.

### 3c. Dead code elimination

Remove ops that produce values never consumed (no subsequent load of
that register, and it's not a memory store).  Particularly useful after
the `static double me_jit_exp10(...)` inline helpers are compiled but
never called.

**Impact:** minor for typical kernels.  **Complexity:** ~100–150 lines
(reverse scan of ops tracking register liveness).

---

## Recommended Roadmap

### Phase A: Basic Block Coalescing (Option 1)
- Biggest bang for the buck
- Moderate complexity (~300 lines)
- Expected result: mandelbrot goes from 0.8× to ~2–4× speedup
- Can be implemented and tested in a single session

### Phase B: Peephole Opts (Option 3a)
- Complement Phase A
- Low complexity (~150 lines)
- Expected additional improvement: ~15–25%
- Stack-based: straightforward to implement

### Phase C: Structured Control Flow (Option 2)
- Only if Phase A+B insufficient
- High complexity (~2000 lines)
- Would bring wasm32 JIT close to native TCC performance
- Consider using binaryen as a post-processing step instead
  (pipe TCC's wasm output through `wasm-opt` for restructuring)

### Alternative: Binaryen Post-Processing

Instead of implementing Option 2 from scratch, pipe the generated
.wasm through binaryen's `wasm-opt`:

```c
tcc_output_file(state, "/tmp/kernel.wasm");
system("wasm-opt -O2 /tmp/kernel.wasm -o /tmp/kernel_opt.wasm");
// load kernel_opt.wasm instead
```

Binaryen's optimizer includes:
- CFG restructuring (eliminates switch-loop patterns)
- Register allocation (reduces locals)
- Dead code elimination
- Constant propagation
- Loop optimizations

**Pros:** zero implementation effort for optimization passes.
**Cons:** external dependency, cold-compile latency (wasm-opt adds
~50–200ms per kernel), harder to bundle in Emscripten builds.

Worth benchmarking before investing in Option 2.

---

## Metrics to Track

| Metric | Current | Target (Phase A) | Target (A+B) | Target (A+B+C) |
|--------|---------|-------------------|---------------|-----------------|
| Mandelbrot speedup vs interpreter | 0.8× | 2–4× | 3–5× | 5–8× |
| Dispatch overhead (% of runtime) | ~40–60% | ~10–15% | ~5–10% | ~0% |
| br_table entries per function | N_ops | N_blocks | N_blocks | 0 |
| Wasm bytecode size (mandelbrot) | ~N×14 | ~N×8 | ~N×6 | ~N×4 |

N = number of IR ops in the kernel function.
