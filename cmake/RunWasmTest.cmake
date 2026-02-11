# CTest driver for tests/wasm/*.c.
# Mirrors tests/wasm/Makefile:
#   - compile with wasm32-tcc
#   - run with Node via run-wasm.mjs
#   - compare stdout with *.expect

cmake_minimum_required(VERSION 3.20)

foreach(_req TEST_NAME WASM_TCC NODE_EXECUTABLE SOURCE_DIR BINARY_DIR)
  if(NOT DEFINED ${_req} OR "${${_req}}" STREQUAL "")
    message(FATAL_ERROR "RunWasmTest.cmake: missing required -D${_req}=...")
  endif()
endforeach()

set(_wasm_src "${SOURCE_DIR}/tests/wasm/${TEST_NAME}.c")
set(_expect_file "${SOURCE_DIR}/tests/wasm/${TEST_NAME}.expect")
set(_runner "${SOURCE_DIR}/tests/wasm/run-wasm.mjs")
set(_out_dir "${BINARY_DIR}/tests/wasm/out")
set(_out_wasm "${_out_dir}/${TEST_NAME}.wasm")
set(_out_txt "${_out_dir}/${TEST_NAME}.out")

foreach(_path _wasm_src _expect_file _runner)
  if(NOT EXISTS "${${_path}}")
    message(FATAL_ERROR "RunWasmTest.cmake: missing input file: ${${_path}}")
  endif()
endforeach()

file(MAKE_DIRECTORY "${_out_dir}")

execute_process(
  COMMAND "${WASM_TCC}" -nostdlib "-B${BINARY_DIR}" "-I${SOURCE_DIR}/include" "${_wasm_src}" -o "${_out_wasm}"
  RESULT_VARIABLE _cc_rv
  OUTPUT_VARIABLE _cc_stdout
  ERROR_VARIABLE _cc_stderr
)
if(NOT _cc_rv EQUAL 0)
  message(FATAL_ERROR
    "Wasm compile failed for ${TEST_NAME}\n"
    "compiler: ${WASM_TCC}\n"
    "stdout:\n${_cc_stdout}\n"
    "stderr:\n${_cc_stderr}")
endif()

execute_process(
  COMMAND "${NODE_EXECUTABLE}" "${_runner}" "${_out_wasm}"
  RESULT_VARIABLE _node_rv
  OUTPUT_VARIABLE _node_stdout
  ERROR_VARIABLE _node_stderr
)
if(NOT _node_rv EQUAL 0)
  message(FATAL_ERROR
    "Wasm run failed for ${TEST_NAME}\n"
    "node: ${NODE_EXECUTABLE}\n"
    "stdout:\n${_node_stdout}\n"
    "stderr:\n${_node_stderr}")
endif()

file(WRITE "${_out_txt}" "${_node_stdout}")
execute_process(
  COMMAND "${CMAKE_COMMAND}" -E compare_files "${_expect_file}" "${_out_txt}"
  RESULT_VARIABLE _cmp_rv
)
if(NOT _cmp_rv EQUAL 0)
  file(READ "${_expect_file}" _expect_contents)
  file(READ "${_out_txt}" _actual_contents)
  message(FATAL_ERROR
    "Unexpected wasm test output for ${TEST_NAME}\n"
    "expected:\n${_expect_contents}\n"
    "actual:\n${_actual_contents}")
endif()
