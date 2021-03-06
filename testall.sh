#!/bin/sh

# Regression testing script for ESPRESSO
# Step through a list of files
#  Compile, run, and check the output of each expected-to-work test
#  Compile and check the error of each expected-to-fail test

# Path to the LLVM interpreter
LLI="lli"
#LLI="/usr/local/opt/llvm/bin/lli"

# Path to the espresso compiler.  Usually "./espresso.native"
# Try "_build/expresso.native" if ocamlbuild was unable to create a symbolic link.
ESPRESSO="./espresso.native"
#ESPRESSO="_build/espresso.native"

# Set time limit for all operations
ulimit -t 30
TMP_DIR="_tmp2"
mkdir -p ${TMP_DIR}
#rm ${TMP_DIR}/*.*
globallog=testall.log
rm -f $globallog
error=0
globalerror=0

keep=0

Usage() {
    echo "Usage: testall.sh [options] [.es files]"
    echo "-k    Keep intermediate files"
    echo "-h    Print this help"
    exit 1
}

SignalError() {
    if [ $error -eq 0 ] ; then
	echo "FAILED"
	error=1
    fi
    echo "  $1"
}

# Compare <outfile> <reffile> <difffile>
# Compares the outfile with reffile.  Differences, if any, written to difffile
Compare() {
    generatedfiles="$generatedfiles $3"
    echo diff -b $1 $2 ">" $3 1>&2
    diff -b "$1" "$2" > "$3" 2>&1 || {
	SignalError "$1 differs"
	echo "FAILED $1 differs from $2" 1>&2
    }
}

# Run <args>
# Report the command, run it, and report any errors
Run() {
    echo $* 1>&2
    eval $* || {
	SignalError "$1 failed on $*"
	return 1
    }
}

# RunFail <args>
# Report the command, run it, and expect an error
RunFail() {
    echo $* 1>&2
    eval $* && {
	SignalError "failed: $* did not report an error"
	return 1
    }
    return 0
}

Check() {
    error=0
    basename=`echo $1 | sed 's/.*\\///
                             s/.es$//'`
    reffile=`echo $1 | sed 's/.es$//'`
    basedir="`echo $1 | sed 's/\/[^\/]*$//'`/."

    echo -n "$basename..."

    echo 1>&2
    echo "###### Testing $basename" 1>&2

    generatedfiles=""
    generatedfiles="$generatedfiles ${TMP_DIR}/${basename}.ll ${TMP_DIR}/${basename}.out" &&
    Run "$ESPRESSO" "-l <" $1 ">" "${TMP_DIR}/${basename}.ll" &&
    Run "$LLI" "${TMP_DIR}/${basename}.ll" ">" "${TMP_DIR}/${basename}.out" &&
    Compare ${TMP_DIR}/${basename}.out ${reffile}.out ${TMP_DIR}/${basename}.diff
    # tr -d "\n" < ${TMP_DIR}/${basename}.ll > ${TMP_DIR}/${basename}.after1
    # tr -d "\t" < ${TMP_DIR}/${basename}.after1 > ${TMP_DIR}/${basename}.after2
    # tr -d " "  < ${TMP_DIR}/${basename}.after2 > ${TMP_DIR}/${basename}.after
    # tr -d "\n" < $1 > ${TMP_DIR}/${basename}.before1
    # tr -d "\t"  < ${TMP_DIR}/${basename}.before1 > ${TMP_DIR}/${basename}.before2
    # tr -d " "  < ${TMP_DIR}/${basename}.before2 > ${TMP_DIR}/${basename}.before      
    # Compare ${TMP_DIR}/${basename}.before ${TMP_DIR}/${basename}.after ${TMP_DIR}/${basename}.diff
    # rm ${TMP_DIR}/${basename}.after1
    # rm ${TMP_DIR}/${basename}.after2
    # rm ${TMP_DIR}/${basename}.before1
    # rm ${TMP_DIR}/${basename}.before2
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

CheckFail() {
    error=0
    basename=`echo $1 | sed 's/.*\\///
                             s/.es$//'`
    reffile=`echo $1 | sed 's/.es$//'`
    basedir="`echo $1 | sed 's/\/[^\/]*$//'`/."

    echo -n "$basename..."


    echo 1>&2
    echo "###### Testing $basename" 1>&2

    generatedfiles=""

    generatedfiles="$generatedfiles ${TMP_DIR}/${basename}.err ${TMP_DIR}/${basename}.diff" &&
    RunFail "$ESPRESSO" "<" $1 "2>" "${TMP_DIR}/${basename}.err" ">>" $globallog &&
    Compare ${TMP_DIR}/${basename}.err ${reffile}.err ${TMP_DIR}/${basename}.diff

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
	    Usage
	    ;;
    esac
done

shift `expr $OPTIND - 1`

LLIFail() {
  echo "Could not find the LLVM interpreter \"$LLI\"."
  echo "Check your LLVM installation and/or modify the LLI variable in testall.sh"
  exit 1
}

which "$LLI" >> $globallog || LLIFail


if [ $# -ge 1 ]
then
    files=$@
else
    files="tests/test_*.es tests/fail_*.es"
fi

for file in $files
do
    case $file in
	*test_*)
	    Check $file 2>> $globallog
	    ;;
	*fail_*)
	    CheckFail $file 2>> $globallog
	    ;;
	*)
	    echo "unknown file type $file"
	    globalerror=1
	    ;;
    esac
done

exit $globalerror
