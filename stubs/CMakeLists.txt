
cmake_minimum_required(VERSION 3.5)

if (NOT TARGET CycloneDDS::ddsc)
    # Find the CycloneDDS package. If it is not in a default location, try
    # finding it relative to the example where it most likely resides.
    find_package(CycloneDDS REQUIRED PATHS "${CMAKE_SOURCE_DIR}/../../")
endif()

include_directories(
        "/usr/local/include"
)

# This is a convenience function, provided by the CycloneDDS package,
# that will supply a library target related the the given idl file.
# In short, it takes the idl file, generates the source files with
# the proper data types and compiles them into a library.
idlc_generate(ddsbit bit.idl)

# Both executables have only one related source file.

add_library(ddstubs SHARED dds_stubs.c)
add_executable(test_stub test_stub.c)
# add_executable(HelloworldSubscriber subscriber.c)

# Both executables need to be linked to the idl data type library and
# the ddsc API library.
target_link_libraries(ddstubs ddsbit CycloneDDS::ddsc)
target_link_libraries(test_stub ddstubs ddsbit CycloneDDS::ddsc)
