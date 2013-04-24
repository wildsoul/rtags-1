cmake_minimum_required(VERSION 2.8.6)
if (NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release)
endif ()

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wformat -Wall")
if (CMAKE_SYSTEM_NAME MATCHES "Linux")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fpic")
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fpic")
endif ()

find_package(Qt4 REQUIRED)
find_package(V8 REQUIRED)
find_package(LibClang REQUIRED)

add_subdirectory(CPlusPlus)
add_subdirectory(rct)
include(${CMAKE_CURRENT_LIST_DIR}/rct/compiler.cmake)
# set(RCT_INCLUDE_DIR rct)
# include(${CMAKE_CURRENT_LIST_DIR}/rct/rct.cmake)

if (CMAKE_BUILD_TYPE MATCHES "Debug")
  add_definitions(-DRTAGS_DEBUG)
endif ()

add_definitions(-Wall)
add_definitions("-DESPRIMA_JS=\"${PROJECT_SOURCE_DIR}/src/esprima/esprima.js\"")
add_definitions(-DOS_${CMAKE_SYSTEM_NAME})

if (CMAKE_SYSTEM_NAME MATCHES "Linux")
  set(SYSTEM_LIBS pthread rt crypto dl)
elseif (CMAKE_SYSTEM_NAME MATCHES "FreeBSD")
  add_definitions(-D__LONG_LONG_SUPPORTED)
  set(SYSTEM_LIBS pthread crypto)
elseif (CMAKE_SYSTEM_NAME MATCHES "Darwin")
  set(SYSTEM_LIBS crypto)
  find_program(SW_VER sw_vers)
  execute_process(COMMAND "${SW_VER}" -productVersion OUTPUT_VARIABLE osver)
  string(REPLACE "\n" "" osver "${osver}")
  if (${osver} VERSION_LESS "10.7")
    message("OS X ${osver} is too old for FSEvents, falling back to kqueue")
    # else ()
    #   set(HAVE_FSEVENTS 1)
  endif ()
  find_library(CORESERVICES_LIBRARY CoreServices)
  find_path(CORESERVICES_INCLUDE "CoreServices/CoreServices.h")
  find_library(COREFOUNDATION_LIBRARY CoreFoundation)
  find_path(COREFOUNDATION_INCLUDE "CoreFoundation/CoreFoundation.h")
endif ()

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/bin)

set(RTAGS_SHARED_SOURCES
  Client.cpp
  CompileMessage.cpp
  CompletionMessage.cpp
  Location.cpp
  QueryMessage.cpp
  RTags.cpp)

add_library(shared ${RTAGS_SHARED_SOURCES})

add_definitions("-DCLANG_INCLUDEPATH=\"${CLANG_SYSTEM_INCLUDE}\"")

set(RDM_SOURCES
  ClangThread.cpp
  FileManager.cpp
  GccArguments.cpp
  Preprocessor.cpp
  Project.cpp
  Server.cpp
  )

include_directories(${CMAKE_CURRENT_LIST_DIR}
                    ${CORESERVICES_INCLUDE}
                    ${COREFOUNDATION_INCLUDE}
                    ${CLANG_INCLUDE}
                    rct/include/
                    rct
                    ${CMAKE_CURRENT_BINARY_DIR}/rct/include/rct)

add_executable(rc rc.cpp RClient.cpp)
add_dependencies(rc rct shared)
target_link_libraries(rc shared rct ${SYSTEM_LIBS})

add_library(rtags SHARED ${RDM_SOURCES})
target_link_libraries(rtags ${CORESERVICES_LIBRARY} ${COREFOUNDATION_LIBRARY} shared rct ${SYSTEM_LIBS} ${CLANG_LIBS})
add_dependencies(rtags rct)

add_executable(rdm rdm.cpp)
target_link_libraries(rdm rtags shared)

set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/bin)

# include_directories(${V8_INCLUDE})
# if (V8_DISPOSE_HAS_ISOLATE)
#   add_definitions(-DV8_DISPOSE_HAS_ISOLATE)
# endif ()
# add_library(rtagsesprima MODULE IndexerJobEsprima.cpp JSParser.cpp)
# add_dependencies(rtagsesprima rtags)
# target_link_libraries(rtagsesprima ${V8_LIBS} rtags)

if (NOT "${PROJECT_SOURCE_DIR}" STREQUAL "${PROJECT_BINARY_DIR}")
  file (GLOB binFiles "${PROJECT_SOURCE_DIR}/bin/*")
  file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/bin")
  message("Copying files from ${PROJECT_SOURCE_DIR}/bin to ${PROJECT_BINARY_DIR}/bin")
  file (COPY ${binFiles} DESTINATION "${PROJECT_BINARY_DIR}/bin")
endif ()

if (CMAKE_SYSTEM_NAME MATCHES "Darwin")
  add_custom_command(TARGET rdm POST_BUILD
    COMMAND ${CMAKE_CURRENT_LIST_DIR}/fix-dylib.sh ${PROJECT_BINARY_DIR} ${PROJECT_BINARY_DIR}/bin/rdm)
  add_custom_command(TARGET rc POST_BUILD
    COMMAND ${CMAKE_CURRENT_LIST_DIR}/fix-dylib.sh ${PROJECT_BINARY_DIR} ${PROJECT_BINARY_DIR}/bin/rc)
endif ()

include(${QT_USE_FILE})
add_definitions(${QT_DEFINITIONS})

set(RPARSER_INCLUDES
    CPlusPlus/src/plugins/cpptools
    CPlusPlus/src/libs
    CPlusPlus/src/libs/3rdparty
    CPlusPlus/src/libs/cplusplus)
include_directories(${RPARSER_INCLUDES})

set(RPARSER_SRCS RParserProject.cpp)
set(RPARSER_MOC_HDRS RParserProject.h)
qt4_wrap_cpp(RPARSER_MOC_SRCS ${RPARSER_MOC_HDRS})

add_library(rtagsrparser MODULE ${RPARSER_SRCS} ${RPARSER_MOC_SRCS})
add_dependencies(rtagsrparser rtags rct CPlusPlus)
target_link_libraries(rtagsrparser rtags ${QT_LIBRARIES} rct CPlusPlus)

set(CLANG_SRCS ClangProject.cpp)
add_library(rtagsclang MODULE ${CLANG_SRCS})
add_dependencies(rtagsclang rtags rct)
target_link_libraries(rtagsclang rtags rct ${CLANG_LIBS})
