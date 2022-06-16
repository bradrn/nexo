cmake_minimum_required(VERSION 3.5)

project(nexo-gui LANGUAGES CXX)

set(CMAKE_INCLUDE_CURRENT_DIR ON)

set(CMAKE_AUTOUIC ON)
set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTORCC ON)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

find_package(QT NAMES Qt6 Qt5 COMPONENTS Widgets LinguistTools REQUIRED)
find_package(Qt${QT_VERSION_MAJOR} COMPONENTS Widgets LinguistTools REQUIRED)

set(NEXO_INTEROP_DIR ${CMAKE_CURRENT_LIST_DIR}/../interop/ CACHE PATH "Path to interop directory of Nexo source code")
find_program(HAS_GHCUP "ghcup")
if(HAS_GHCUP)
    execute_process(COMMAND ghcup run -- sh -c "dirname $(cut -d ' ' -f 3 < $(which ghc).shim)" OUTPUT_VARIABLE GHC_BIN_DIR OUTPUT_STRIP_TRAILING_WHITESPACE)
else() # try Stack if no GHCup
    execute_process(COMMAND stack path --compiler-bin OUTPUT_VARIABLE GHC_BIN_DIR OUTPUT_STRIP_TRAILING_WHITESPACE)
endif()
set(GHC_INCLUDE_DIR ${GHC_BIN_DIR}/../lib/include CACHE PATH "Path to includes directory of GHC")

include_directories(${NEXO_INTEROP_DIR}/stub ${GHC_INCLUDE_DIR})

set(TS_FILES nexo-gui_en_AU.ts)

add_executable(nexo-gui
  main.cpp
  mainwindow.cpp
  mainwindow.h
  hssheet.cpp
  hssheet.h
  value.cpp
  value.h
  hsvalue.cpp
  hsvalue.h
  hscell.cpp
  hscell.h
  inputlist.cpp
  inputlist.h
  tablemodel.cpp
  tablemodel.h
  tabledelegate.cpp
  tabledelegate.h
  table.cpp
  table.h
  ${TS_FILES}
)

target_link_libraries(nexo-gui
  PRIVATE Qt${QT_VERSION_MAJOR}::Widgets
  ${NEXO_INTEROP_DIR}/nexo-interop.a ws2_32 psapi dbghelp winmm shlwapi)

qt_create_translation(QM_FILES ${CMAKE_SOURCE_DIR} ${TS_FILES})
