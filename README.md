# MiniCC

A small fork of the [Tiny C Compiler](https://repo.or.cz/w/tinycc.git) (TinyCC)
trimmed down and extended to be **embedded inside other applications as a
library**. It keeps TinyCC's defining trait — compile *and run* C code on the
fly, in-process — and adds two things TinyCC lacks: a **wasm32 backend** and a
proper **shared-library (`libtcc.so`/`.dylib`) build** driven by CMake.

> Status: **beta**. Used internally in python-blosc2; interfaces are
> stabilizing but may still change.

## What's different from TinyCC

- **wasm32 backend.** Compile C straight to freestanding WebAssembly
  (`wasm32-unknown-unknown`, no WASI, no host OS runtime), aimed at numeric
  kernels you want to JIT and run anywhere. Includes optimization passes —
  basic-block coalescing, peephole, direct branches, and structured
  control-flow reconstruction — to keep loop-heavy code competitive. See
  `wasm-specs.md` and `wasm32-opts.md` for the design and benchmarks.
- **Shared `libtcc`.** Builds a real `libtcc.so`/`libtcc.dylib` with the
  `libtcc1` runtime helpers merged in, so apps can link one library and use the
  `libtcc.h` API to compile and run C at runtime.
- **CMake build.** Replaces the autotools/Makefile flow, with cross-compilation
  support (`CMAKE_CROSSCOMPILING`) and options to pick the native backend.
- **Backend fixes.** Assorted arm64 and x86_64 correctness fixes carried on top
  of upstream TinyCC.

Everything else is TinyCC: native code generation for i386, x86_64, arm, arm64
and riscv64, the full C preprocessor, and `-run` to execute C without an
explicit link step.

## Building

```sh
cmake -S . -B build
cmake --build build
```

Useful options (all `ON` by default):

| Option | What it does |
|--------|--------------|
| `MINICC_BUILD_WASM32_TCC`   | Build the `wasm32-tcc` cross compiler |
| `MINICC_BUILD_SHARED_LIBTCC`| Build shared `libtcc` (`.so`/`.dylib`) |
| `MINICC_ENABLE_TESTING`     | Enable the CTest harness |
| `MINICC_NATIVE_TARGET`      | Force the native backend (`i386`, `x86_64`, `arm`, `arm64`, `riscv64`, `wasm32`) |

Run the tests with `ctest --test-dir build`.

## Using it as a library

Link against `libtcc` and drive it through `libtcc.h` — create a `TCCState`,
set the output type, compile a string, and either relocate and call a symbol or
`tcc_run()` it. See `tests/libtcc_api_test.c` for a minimal working example.

## License

LGPL, same as TinyCC. See `COPYING` and `RELICENSING`.
