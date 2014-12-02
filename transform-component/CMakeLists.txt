set(LLVM_LINK_COMPONENTS support)
set(LLVM_USED_LIBS clangTooling clangBasic clangAST)
set(CMAKE_CXX_FLAGS "-std=c++11 -stdlib=libc++")
set(CMAKE_EXE_LINKER_FLAGS "-stdlib=libc++ -std=c++11")

add_clang_executable(transform-component
  TransformComponent.cpp
  )
target_link_libraries(transform-component
  clangTooling
  clangBasic
  clangASTMatchers
  )
