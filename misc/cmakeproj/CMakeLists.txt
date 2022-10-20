cmake_minimum_required(VERSION 3.10)

# set the project name
project(ficus_testproj)

# if you want to debug a concrete app or sample,
# written in Ficus, rather than Ficus compiler, change the path name
file(GLOB srcs ${CMAKE_CURRENT_SOURCE_DIR}/../../__fxbuild__/ficus/*.c)

set(runtime_h ${CMAKE_CURRENT_SOURCE_DIR}/../../runtime/ficus/ficus.h)
file(GLOB runtime_impl ${CMAKE_CURRENT_SOURCE_DIR}/../../runtime/ficus/impl/*.h)
file(GLOB runtime_impl_c ${CMAKE_CURRENT_SOURCE_DIR}/../../runtime/ficus/impl/*.c)

include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../../runtime)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../../lib/Protobuf)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../../lib/Onnx)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../../lib/NN)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../../lib/Image)

if(APPLE)
link_directories(${CMAKE_CURRENT_SOURCE_DIR}/../../runtime/lib/macos_x64)
endif()

# add the executable
set(the_target ficus_testproj)
add_executable(${the_target} ${srcs} ${runtime_h} ${runtime_impl} ${runtime_impl_c})
if(APPLE)
target_link_libraries(${the_target} "-lomp")
endif()
