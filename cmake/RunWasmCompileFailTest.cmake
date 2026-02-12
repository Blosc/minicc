cmake_minimum_required(VERSION 3.20)

foreach(_req TEST_NAME WASM_TCC SOURCE_DIR BINARY_DIR NODE_EXECUTABLE)
  if(NOT DEFINED ${_req} OR "${${_req}}" STREQUAL "")
    message(FATAL_ERROR "RunWasmCompileFailTest.cmake: missing required -D${_req}=...")
  endif()
endforeach()

set(_wasm_src "${SOURCE_DIR}/tests/wasm/${TEST_NAME}.c")
set(_out_dir "${BINARY_DIR}/tests/wasm/out")
set(_out_wasm "${_out_dir}/${TEST_NAME}.wasm")

if(NOT EXISTS "${_wasm_src}")
  message(FATAL_ERROR "RunWasmCompileFailTest.cmake: missing input file: ${_wasm_src}")
endif()

file(MAKE_DIRECTORY "${_out_dir}")
file(REMOVE "${_out_wasm}")

execute_process(
  COMMAND "${WASM_TCC}" -nostdlib "-B${BINARY_DIR}" "-I${SOURCE_DIR}/include" "${_wasm_src}" -o "${_out_wasm}"
  RESULT_VARIABLE _cc_rv
  OUTPUT_VARIABLE _cc_stdout
  ERROR_VARIABLE _cc_stderr
)

if(NOT _cc_rv EQUAL 0)
  message(FATAL_ERROR
    "Expected wasm compile to succeed for ${TEST_NAME}, but it failed.\n"
    "exit code: ${_cc_rv}\n"
    "stdout:\n${_cc_stdout}\n"
    "stderr:\n${_cc_stderr}")
endif()

execute_process(
  COMMAND "${NODE_EXECUTABLE}" -e
    "const fs=require('fs');const p=process.argv[1];const m=new WebAssembly.Module(fs.readFileSync(p));const imports=WebAssembly.Module.imports(m);const ok=imports.some(i=>i.kind==='function'&&i.module==='env'&&i.name==='missing');if(!ok){console.error(JSON.stringify(imports));process.exit(2);}"
    "${_out_wasm}"
  RESULT_VARIABLE _node_rv
  OUTPUT_VARIABLE _node_stdout
  ERROR_VARIABLE _node_stderr
)

if(NOT _node_rv EQUAL 0)
  message(FATAL_ERROR
    "Expected wasm import env.missing for ${TEST_NAME}.\n"
    "node exit code: ${_node_rv}\n"
    "stdout:\n${_node_stdout}\n"
    "stderr:\n${_node_stderr}")
endif()
