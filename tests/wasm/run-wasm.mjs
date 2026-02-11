import { readFileSync } from "node:fs";

const [, , wasmPath, exportName = "main"] = process.argv;

if (!wasmPath) {
  console.error("usage: node run-wasm.mjs <module.wasm> [export]");
  process.exit(2);
}

const bytes = readFileSync(wasmPath);
const { instance } = await WebAssembly.instantiate(bytes, {});

if (!(exportName in instance.exports)) {
  throw new Error(`missing wasm export: ${exportName}`);
}

const result = instance.exports[exportName]();
process.stdout.write(String(result) + "\n");
