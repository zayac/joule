#!/bin/sh

DERIVE_BIN=/Users/pzv/code/clang-llvm/llvm/build/bin/derive-terms
TRANSFORM_COMP_BIN=/Users/pzv/code/clang-llvm/llvm/build/bin/transform-component
CONSTRAINT_GEN_BIN=./constraint-generator/main.native
SOLVER=./joule/joule.native
CODE_GENERATOR=./code-generator/main.native

dir=""
filename=""
usage=$"Usage: $0 {clean|build} [filename]"
no_solution_string="No solution is found"

function test {
    "$@"
    local status=$?
    if [ $status -ne 0 ]; then
        echo "error with $1" >&2
    fi
    return $status
}

clean_tool() {
    echo "Cleaning working directory..."
    mv $dir/environment.terms $dir/environment.terms.keep
    rm -f $dir/*.transformed.cpp $dir/*.terms
    mv $dir/environment.terms.keep $dir/environment.terms
    rm -f $dir/*.constraints $dir/*.solution
    rm -f $dir/*.json
    rm -f $dir/*CAL_FI_variables.h
    rm -f $dir/code-hash
}

build_tool () {
    clean_tool
    echo "Reading net list file \"$filename\"..."
    files=()
    while read line; do
        if [[ $line =~ ([[:alpha:]]*)\@[[:digit:]]+[[:space:]][[:digit:]]+\@([[:alpha:]]*) ]]
        then
            if [ "${BASH_REMATCH[1]}" != "environment" ]
            then
                files+=("${BASH_REMATCH[1]}")
            fi
            if [ "${BASH_REMATCH[2]}" != "environment" ]
            then
                files+=("${BASH_REMATCH[2]}")
            fi
        fi
    done <$filename
    unique_files=($(echo "${files[@]}" | tr ' ' '\n' | sort -u | tr '\n' ' '))
    for file in ${unique_files[@]}
    do
        echo "Transforming source file \"$file.cpp\"..."
        test $TRANSFORM_COMP_BIN $dir/$file.cpp -- -stdlib=libc++ -std=c++11 -I/usr/local/include -I/usr/include -I/usr/local/Cellar/gcc/4.9.2_1/include/c++/4.9.2 -I/usr/local/Cellar/gcc/4.9.2_1/include/c++/4.9.2/x86_64-apple-darwin14.0.0
        echo "Deriving interface from transformed source file \"$file.transformed.cpp\"..."
        test $DERIVE_BIN $dir/$file.transformed.cpp -- -stdlib=libc++ -std=c++11 -I/usr/local/include -I/usr/include -I/usr/local/Cellar/gcc/4.9.2_1/include/c++/4.9.2 -I/usr/local/Cellar/gcc/4.9.2_1/include/c++/4.9.2/x86_64-apple-darwin14.0.0
    done
    echo "Deriving constraints..."
    test $CONSTRAINT_GEN_BIN $filename
    echo "Solving constraints in \"${filename%.*}.constraints\"..."
    test $SOLVER ${filename%.*}.constraints | tee ${filename%.*}.solution
    if grep -q "$no_solution_string" "${filename%.*}.solution"; then
        echo "Error: constraint resolution failed. Cannot generate code"
        exit 1
    else
        echo "Generating code for the solution \"${filename%.*}.solution\"..."
        $CODE_GENERATOR ${filename%.*}.solution
    fi
}

if [ -z "$2" ]
then
    echo $usage
    exit 1
fi
filename=$2
dir=$(dirname $filename)

case "$1" in
    build)
        
        build_tool
        ;;
    clean)
        clean_tool
        ;;
    *)
        echo $usage
        exit 1
esac
