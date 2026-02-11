cmake_minimum_required(VERSION 3.20)

foreach(_req TEST_NAME WASM_TCC SOURCE_DIR BINARY_DIR)
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

if(_cc_rv EQUAL 0)
  message(FATAL_ERROR
    "Expected wasm compile to fail for ${TEST_NAME}, but it succeeded.\n"
    "stdout:\n${_cc_stdout}\n"
    "stderr:\n${_cc_stderr}")
endif()

if(NOT _cc_stderr MATCHES "unresolved direct call")
  message(FATAL_ERROR
    "Expected unresolved direct call diagnostic for ${TEST_NAME}.\n"
    "exit code: ${_cc_rv}\n"
    "stdout:\n${_cc_stdout}\n"
    "stderr:\n${_cc_stderr}")
endif()
