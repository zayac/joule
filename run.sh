#!/bin/sh

BIN_DIR=.
DERIVE_BIN=derive-terms/derive-terms
TRANSFORM_COMP_BIN=transform-component/transform-component
CONSTRAINT_GEN_BIN=constraint-generator/main.native
SOLVER=joule/joule.native
CODE_GENERATOR=code-generator/main.native

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
        test $BIN_DIR/$TRANSFORM_COMP_BIN $dir/$file.cpp --
        echo "Deriving interface from transformed source file \"$file.transformed.cpp\"..."
        test $BIN_DIR/$DERIVE_BIN $dir/$file.transformed.cpp --
    done
    echo "Deriving constraints..."
    test $BIN_DIR/$CONSTRAINT_GEN_BIN $filename
    echo "Solving constraints in \"${filename%.*}.constraints\"..."
    test $BIN_DIR/$SOLVER ${filename%.*}.constraints | tee ${filename%.*}.solution
    if grep -q "$no_solution_string" "${filename%.*}.solution"; then
        echo "Error: constraint resolution failed. Cannot generate code"
        exit 1
    else
        echo "Generating code for the solution \"${filename%.*}.solution\"..."
        $BIN_DIR/$CODE_GENERATOR ${filename%.*}.solution
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
