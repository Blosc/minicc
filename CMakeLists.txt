cmake_minimum_required(VERSION 3.20)

project(minicc C ASM)

set(CMAKE_C_STANDARD 99)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_C_EXTENSIONS ON)

option(MINICC_BUILD_WASM32_TCC "Build wasm32-tcc cross compiler" ON)
option(MINICC_BUILD_SHARED_LIBTCC "Build shared libtcc (libtcc.so/libtcc.dylib)" ON)
option(MINICC_ENABLE_TESTING "Enable CTest-based test harness" ON)
set(MINICC_NATIVE_TARGET "" CACHE STRING "Native backend target (auto, i386, x86_64, arm, arm64, riscv64, wasm32)")

set_property(CACHE MINICC_NATIVE_TARGET PROPERTY STRINGS "" i386 x86_64 arm arm64 riscv64 wasm32)

file(READ "${CMAKE_CURRENT_SOURCE_DIR}/VERSION" MINICC_VERSION)
string(STRIP "${MINICC_VERSION}" MINICC_VERSION)

if(CMAKE_C_COMPILER_ID MATCHES "Clang")
  set(MINICC_CONFIG_CC_NAME "clang")
elseif(CMAKE_C_COMPILER_ID STREQUAL "GNU")
  set(MINICC_CONFIG_CC_NAME "gcc")
elseif(CMAKE_C_COMPILER_ID STREQUAL "MSVC")
  set(MINICC_CONFIG_CC_NAME "msvc")
elseif(CMAKE_C_COMPILER_ID STREQUAL "TinyCC")
  set(MINICC_CONFIG_CC_NAME "tcc")
else()
  set(MINICC_CONFIG_CC_NAME "gcc")
endif()

set(MINICC_GCC_MAJOR 0)
set(MINICC_GCC_MINOR 0)
if(CMAKE_C_COMPILER_VERSION)
  string(REPLACE "." ";" _minicc_cc_ver "${CMAKE_C_COMPILER_VERSION}")
  list(LENGTH _minicc_cc_ver _minicc_cc_ver_len)
  if(_minicc_cc_ver_len GREATER 0)
    list(GET _minicc_cc_ver 0 MINICC_GCC_MAJOR)
  endif()
  if(_minicc_cc_ver_len GREATER 1)
    list(GET _minicc_cc_ver 1 MINICC_GCC_MINOR)
  endif()
endif()

set(MINICC_CONFIG_TCCDIR "${CMAKE_INSTALL_PREFIX}/lib/tcc")
string(REPLACE "\\" "/" MINICC_CONFIG_TCCDIR "${MINICC_CONFIG_TCCDIR}")

set(MINICC_DEFINE_CONFIG_SYSROOT 0)
if(CMAKE_SYSROOT)
  set(MINICC_DEFINE_CONFIG_SYSROOT 1)
  set(MINICC_CONFIG_SYSROOT "${CMAKE_SYSROOT}")
  string(REPLACE "\\" "/" MINICC_CONFIG_SYSROOT "${MINICC_CONFIG_SYSROOT}")
endif()

set(MINICC_DEFINE_CONFIG_USR_INCLUDE 0)
if(CMAKE_SYSROOT)
  set(MINICC_DEFINE_CONFIG_USR_INCLUDE 1)
  set(MINICC_CONFIG_USR_INCLUDE "${CMAKE_SYSROOT}/usr/include")
elseif(APPLE)
  execute_process(
    COMMAND xcrun --show-sdk-path
    OUTPUT_VARIABLE _minicc_sdk_path
    ERROR_QUIET
    OUTPUT_STRIP_TRAILING_WHITESPACE
  )
  if(_minicc_sdk_path)
    set(MINICC_DEFINE_CONFIG_USR_INCLUDE 1)
    set(MINICC_CONFIG_USR_INCLUDE "${_minicc_sdk_path}/usr/include")
  endif()
endif()

if(MINICC_DEFINE_CONFIG_USR_INCLUDE)
  string(REPLACE "\\" "/" MINICC_CONFIG_USR_INCLUDE "${MINICC_CONFIG_USR_INCLUDE}")
endif()

set(MINICC_DEFINE_CONFIG_DWARF_VERSION 0)
set(MINICC_DEFINE_CONFIG_CODESIGN 0)
if(APPLE)
  set(MINICC_DEFINE_CONFIG_DWARF_VERSION 1)
  set(MINICC_CONFIG_DWARF_VERSION 4)
  set(MINICC_DEFINE_CONFIG_CODESIGN 1)
endif()

set(MINICC_DEFINE_CONFIG_OS_RELEASE 0)
if(CMAKE_SYSTEM_VERSION)
  set(MINICC_DEFINE_CONFIG_OS_RELEASE 1)
  set(MINICC_CONFIG_OS_RELEASE "${CMAKE_SYSTEM_VERSION}")
endif()

configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/cmake/config.h.in"
  "${CMAKE_CURRENT_BINARY_DIR}/config.h"
  @ONLY
)

add_executable(minicc-c2str conftest.c)
target_compile_definitions(minicc-c2str PRIVATE C2STR=1)

set(MINICC_TCCDEFS "${CMAKE_CURRENT_BINARY_DIR}/tccdefs_.h")
add_custom_command(
  OUTPUT "${MINICC_TCCDEFS}"
  COMMAND minicc-c2str "${CMAKE_CURRENT_SOURCE_DIR}/include/tccdefs.h" "${MINICC_TCCDEFS}"
  DEPENDS minicc-c2str "${CMAKE_CURRENT_SOURCE_DIR}/include/tccdefs.h"
  VERBATIM
)
add_custom_target(minicc-predefs DEPENDS "${MINICC_TCCDEFS}")

set(MINICC_LIBTCC_CORE_SOURCES
  libtcc.c
  tccpp.c
  tccgen.c
  tccdbg.c
  tccasm.c
  tccelf.c
  tccrun.c
)

function(minicc_backend_for_target target out_sources out_defines)
  set(_srcs)
  set(_defs)
  if(target STREQUAL "i386")
    list(APPEND _defs TCC_TARGET_I386)
    list(APPEND _srcs i386-gen.c i386-link.c i386-asm.c)
  elseif(target STREQUAL "x86_64")
    list(APPEND _defs TCC_TARGET_X86_64)
    list(APPEND _srcs x86_64-gen.c x86_64-link.c i386-asm.c)
  elseif(target STREQUAL "arm")
    list(APPEND _defs TCC_TARGET_ARM TCC_ARM_EABI TCC_ARM_HARDFLOAT TCC_ARM_VFP)
    list(APPEND _srcs arm-gen.c arm-link.c arm-asm.c)
  elseif(target STREQUAL "arm64")
    list(APPEND _defs TCC_TARGET_ARM64)
    list(APPEND _srcs arm64-gen.c arm64-link.c arm64-asm.c)
  elseif(target STREQUAL "riscv64")
    list(APPEND _defs TCC_TARGET_RISCV64)
    list(APPEND _srcs riscv64-gen.c riscv64-link.c riscv64-asm.c)
  elseif(target STREQUAL "wasm32")
    list(APPEND _defs TCC_TARGET_WASM32)
    list(APPEND _srcs wasm-gen.c wasm-link.c tccwasm.c)
  else()
    message(FATAL_ERROR "Unsupported MiniCC backend target: ${target}")
  endif()

  set(${out_sources} "${_srcs}" PARENT_SCOPE)
  set(${out_defines} "${_defs}" PARENT_SCOPE)
endfunction()

function(minicc_object_format out_sources out_defines)
  set(_srcs)
  set(_defs)
  if(WIN32)
    list(APPEND _defs TCC_TARGET_PE)
    list(APPEND _srcs tccpe.c)
  elseif(APPLE)
    list(APPEND _defs TCC_TARGET_MACHO)
    list(APPEND _srcs tccmacho.c)
  endif()

  set(${out_sources} "${_srcs}" PARENT_SCOPE)
  set(${out_defines} "${_defs}" PARENT_SCOPE)
endfunction()

function(minicc_libtcc1_helpers_for_target target out_sources)
  set(_srcs)

  if(target STREQUAL "i386")
    list(APPEND _srcs lib/libtcc1.c)
  elseif(target STREQUAL "x86_64")
    list(APPEND _srcs lib/libtcc1.c)
  elseif(target STREQUAL "arm")
    list(APPEND _srcs lib/libtcc1.c lib/armeabi.c lib/armflush.c)
  elseif(target STREQUAL "arm64")
    list(APPEND _srcs lib/lib-arm64.c)
  elseif(target STREQUAL "riscv64")
    list(APPEND _srcs lib/lib-arm64.c)
  else()
    message(FATAL_ERROR "Unsupported MiniCC target for libtcc1 helpers: ${target}")
  endif()

  set(${out_sources} "${_srcs}" PARENT_SCOPE)
endfunction()

set(_minicc_target "${MINICC_NATIVE_TARGET}")
if(_minicc_target STREQUAL "")
  string(TOLOWER "${CMAKE_SYSTEM_PROCESSOR}" _minicc_proc)
  if(_minicc_proc MATCHES "^(x86_64|amd64)$")
    set(_minicc_target "x86_64")
  elseif(_minicc_proc MATCHES "^(i[3-6]86|x86)$")
    set(_minicc_target "i386")
  elseif(_minicc_proc MATCHES "^(aarch64|arm64)$")
    set(_minicc_target "arm64")
  elseif(_minicc_proc MATCHES "^arm")
    set(_minicc_target "arm")
  elseif(_minicc_proc MATCHES "riscv64")
    set(_minicc_target "riscv64")
  else()
    message(FATAL_ERROR "Cannot auto-detect MiniCC target for processor '${CMAKE_SYSTEM_PROCESSOR}'. Set MINICC_NATIVE_TARGET explicitly.")
  endif()
endif()
string(TOLOWER "${_minicc_target}" _minicc_target)

minicc_backend_for_target("${_minicc_target}" MINICC_NATIVE_BACKEND_SOURCES MINICC_NATIVE_DEFINES)
minicc_object_format(MINICC_OBJFMT_SOURCES MINICC_OBJFMT_DEFINES)

set(MINICC_NATIVE_LIBTCC_SOURCES
  ${MINICC_LIBTCC_CORE_SOURCES}
  ${MINICC_NATIVE_BACKEND_SOURCES}
  ${MINICC_OBJFMT_SOURCES}
)

add_library(minicc_libtcc STATIC ${MINICC_NATIVE_LIBTCC_SOURCES})
set_target_properties(minicc_libtcc PROPERTIES OUTPUT_NAME "tcc")
target_include_directories(minicc_libtcc PRIVATE "${CMAKE_CURRENT_BINARY_DIR}" "${CMAKE_CURRENT_SOURCE_DIR}")
target_compile_definitions(minicc_libtcc PRIVATE ONE_SOURCE=0 ${MINICC_NATIVE_DEFINES} ${MINICC_OBJFMT_DEFINES})
add_dependencies(minicc_libtcc minicc-predefs)

add_executable(tcc tcc.c)
target_include_directories(tcc PRIVATE "${CMAKE_CURRENT_BINARY_DIR}" "${CMAKE_CURRENT_SOURCE_DIR}")
target_compile_definitions(tcc PRIVATE ONE_SOURCE=0 ${MINICC_NATIVE_DEFINES} ${MINICC_OBJFMT_DEFINES})
target_link_libraries(tcc PRIVATE minicc_libtcc)

set(MINICC_SYSTEM_LIBS)
if(NOT WIN32)
  list(APPEND MINICC_SYSTEM_LIBS m)
endif()
if(NOT WIN32 AND NOT APPLE)
  find_library(MINICC_DL_LIB dl)
  if(MINICC_DL_LIB)
    list(APPEND MINICC_SYSTEM_LIBS "${MINICC_DL_LIB}")
  endif()
endif()
find_package(Threads QUIET)
if(Threads_FOUND)
  list(APPEND MINICC_SYSTEM_LIBS Threads::Threads)
endif()

if(MINICC_SYSTEM_LIBS)
  target_link_libraries(tcc PRIVATE ${MINICC_SYSTEM_LIBS})
endif()

if(MINICC_BUILD_SHARED_LIBTCC)
  minicc_libtcc1_helpers_for_target("${_minicc_target}" MINICC_LIBTCC1_HELPER_SOURCES)
  add_library(minicc_libtcc_shared SHARED ${MINICC_NATIVE_LIBTCC_SOURCES} ${MINICC_LIBTCC1_HELPER_SOURCES})
  set_target_properties(minicc_libtcc_shared PROPERTIES OUTPUT_NAME "tcc")
  target_include_directories(minicc_libtcc_shared PRIVATE "${CMAKE_CURRENT_BINARY_DIR}" "${CMAKE_CURRENT_SOURCE_DIR}")
  target_compile_definitions(minicc_libtcc_shared PRIVATE ONE_SOURCE=0 TCC_LIBTCC1="" ${MINICC_NATIVE_DEFINES} ${MINICC_OBJFMT_DEFINES})
  if(APPLE AND _minicc_target STREQUAL "arm64")
    set_source_files_properties(lib/lib-arm64.c PROPERTIES COMPILE_OPTIONS "-Wno-builtin-memcpy-chk-size")
  endif()
  if(WIN32)
    target_compile_definitions(minicc_libtcc_shared PRIVATE LIBTCC_AS_DLL)
  endif()
  add_dependencies(minicc_libtcc_shared minicc-predefs)
  if(MINICC_SYSTEM_LIBS)
    target_link_libraries(minicc_libtcc_shared PRIVATE ${MINICC_SYSTEM_LIBS})
  endif()
endif()

if(MINICC_BUILD_WASM32_TCC)
  minicc_backend_for_target("wasm32" MINICC_WASM_BACKEND_SOURCES MINICC_WASM_DEFINES)
  add_executable(wasm32-tcc tcc.c)
  target_include_directories(wasm32-tcc PRIVATE "${CMAKE_CURRENT_BINARY_DIR}" "${CMAKE_CURRENT_SOURCE_DIR}")
  target_compile_definitions(wasm32-tcc PRIVATE
    CONFIG_TCC_CROSSPREFIX="wasm32-"
    ${MINICC_WASM_DEFINES}
  )
  add_dependencies(wasm32-tcc minicc-predefs)
  if(MINICC_SYSTEM_LIBS)
    target_link_libraries(wasm32-tcc PRIVATE ${MINICC_SYSTEM_LIBS})
  endif()
endif()

if(MINICC_ENABLE_TESTING)
  include(CTest)
  add_test(NAME minicc.version COMMAND $<TARGET_FILE:tcc> -v)

  if(TARGET wasm32-tcc)
    find_program(MINICC_NODE_EXECUTABLE node)
    if(MINICC_NODE_EXECUTABLE)
      foreach(_wasm_test ll_helpers call_indirect globals static_ptr_init)
        add_test(
          NAME minicc.wasm.${_wasm_test}
          COMMAND ${CMAKE_COMMAND}
            -DTEST_NAME=${_wasm_test}
            -DWASM_TCC=$<TARGET_FILE:wasm32-tcc>
            -DNODE_EXECUTABLE=${MINICC_NODE_EXECUTABLE}
            -DSOURCE_DIR=${CMAKE_CURRENT_SOURCE_DIR}
            -DBINARY_DIR=${CMAKE_CURRENT_BINARY_DIR}
            -P ${CMAKE_CURRENT_SOURCE_DIR}/cmake/RunWasmTest.cmake
        )
      endforeach()
    endif()
  endif()
endif()

message(STATUS "MiniCC target: ${_minicc_target}")
