cmake_minimum_required(VERSION 3.5)

project(supercell-gui LANGUAGES CXX)

set(CMAKE_INCLUDE_CURRENT_DIR ON)

set(CMAKE_AUTOUIC ON)
set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTORCC ON)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

find_package(QT NAMES Qt6 Qt5 COMPONENTS Widgets LinguistTools REQUIRED)
find_package(Qt${QT_VERSION_MAJOR} COMPONENTS Widgets LinguistTools PrintSupport REQUIRED)

set(SUPERCELL_INTEROP_DIR ${CMAKE_CURRENT_LIST_DIR}/../interop/ CACHE PATH "Path to interop directory of Supercell source code")
execute_process(COMMAND stack path --compiler-bin OUTPUT_VARIABLE GHC_BIN_DIR OUTPUT_STRIP_TRAILING_WHITESPACE)
set(GHC_INCLUDE_DIR ${GHC_BIN_DIR}/../lib/include CACHE PATH "Path to includes directory of GHC")

include_directories(${SUPERCELL_INTEROP_DIR}/stub ${GHC_INCLUDE_DIR})

set(TS_FILES supercell-gui_en_AU.ts)

add_executable(supercell-gui
  main.cpp
  mainwindow.cpp
  mainwindow.h
  hssheet.cpp
  hssheet.h
  value.cpp
  value.h
  hsvalue.cpp
  hsvalue.h
  inputlist.cpp
  inputlist.h
  ${TS_FILES}
)

target_link_libraries(supercell-gui
  PRIVATE Qt${QT_VERSION_MAJOR}::Widgets Qt${QT_VERSION_MAJOR}::PrintSupport
  ${SUPERCELL_INTEROP_DIR}/supercell-interop.a ws2_32 psapi dbghelp winmm)

qt_create_translation(QM_FILES ${CMAKE_SOURCE_DIR} ${TS_FILES})
