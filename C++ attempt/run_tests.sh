#!/bin/bash

parser="./parser"

# Set time limit for all operations
ulimit -t 10

globallog=testall.log
rm -f $globallog
error=0
globalerror=0

usage() {
    echo "Usage: testall.sh [options] [.sum files]"
    echo "-h    Print this help"
    exit 1
}

signal_error() {
    if [ $error -eq 0 ] ; then
    echo "FAILED"
    error=1
    fi
    echo "  $1"
}

# Run <args>
# Report the command, run it, and report any errors
run() {
    echo $@
    eval $@ || {
    SignalError "$1 failed on $*"
    return 1
    }
}


check_parser() {
    error=0
    basename=`echo $1 | sed 's/.*\\///
                             s/.sum//'`
    reffile=`echo $1 | sed 's/.sum$//'`
    basedir="`echo $1 | sed 's/\/[^\/]*$//'`/."
    echo -n "$basename..."

    echo 1>&2
    echo "###### Testing $basename" 1>&2

    Run "cat" $1 "|" "$summa"

    if [ $error -eq 0 ] ; then
    if [ $keep -eq 0 ] ; then
        rm -f $generatedfiles
    fi
    echo "OK"
    echo "###### SUCCESS" 1>&2
    else
    echo "###### FAILED" 1>&2
    globalerror=$error
    fi
}

check_fail() {
    error=0
    basename=`echo $1 | sed 's/.*\\///
                             s/.sum//'`
    reffile=`echo $1 | sed 's/.sum$//'`
    basedir="`echo $1 | sed 's/\/[^\/]*$//'`/."

    echo -n "$basename..."

    echo 1>&2
    echo "###### Testing $basename" 1>&2

    generatedfiles=""

    generatedfiles="$generatedfiles ${basename}.s.out" &&
    {Run "$summa" "-s" $1 "2>" ${basename}.s.out} &&
    Compare ${basename}.s.out ${reffile}.out ${basename}.s.diff

    # Report the status and clean up the generated files

    if [ $error -eq 0 ] ; then
    if [ $keep -eq 0 ] ; then
        rm -f $generatedfiles
    fi
    echo "OK"
    echo "###### SUCCESS" 1>&2
    else
    echo "###### FAILED" 1>&2
    globalerror=$error
    fi
}

while getopts kdpsh c; do
    case $c in
    k) # Keep intermediate files
        keep=1
        ;;
    h) # Help
        usage
        ;;
    esac
done

shift `expr $OPTIND - 1`

if [ $# -ge 1 ]
then
    files=$@
else
    files="tests/test-*.gq"
fi

files="examples/*.sum"

for file in $files
do
    case $file in
    *fail*)
        check_fail $file 2>> $globallog
        ;;
    *)
        check_parser $file 2>> $globallog
        ;;
    esac
done

exit $globalerror
