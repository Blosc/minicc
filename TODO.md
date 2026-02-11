# TODO

## Warning Cleanup

- [ ] Replace the Apple arm64 compile-flag suppression (`-Wno-builtin-memcpy-chk-size`) for `lib/lib-arm64.c` with a source-level fix.
  Current suppression is intentionally scoped to the shared `libtcc` CMake path; revisit once `miniexpr` integration stabilizes.
