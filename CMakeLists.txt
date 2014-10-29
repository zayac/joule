set(LLVM_LINK_COMPONENTS support)
set(LLVM_USED_LIBS clangTooling clangBasic clangAST)
set(CMAKE_CXX_FLAGS "-std=c++11 -stdlib=libc++")
set(CMAKE_EXE_LINKER_FLAGS "-stdlib=libc++ -std=c++11")

add_clang_executable(derive-terms
  DeriveTerms.cpp
  Interface.cpp
  )
target_link_libraries(derive-terms
  clangTooling
  clangBasic
  clangASTMatchers
  )
