set(LLVM_LINK_COMPONENTS support)
set(LLVM_USED_LIBS clangTooling clangBasic clangAST)

add_clang_executable(derive-terms
  TermConverter.cpp
  DeriveTerms.cpp
  )
target_link_libraries(derive-terms
  clangTooling
  clangBasic
  clangASTMatchers
  )
