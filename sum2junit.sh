#!/bin/bash

# sum2junit.sh -- convert a .sum file into Junit-compatible XML.
#
# Copyright (C) 2016 Free Software Foundation, Inc.
#
# This file is part of DejaGnu.
#
# DejaGnu is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.

# balasr: Note that this is the upstream patched version that counts failures
# correctly.
# Also additionally we don't make a failure entry for XFAILd tests

if test x"$1" = x; then
  outfile="/tmp/testrun.xml"
  infile="/tmp/testrun.sum"
else
  outfile=${1//\.sum.*/.xml}
  infile=$1
fi

# Where to put the output file
if test x"$2" = x; then
  outfile=${outfile}
else
  outfile="/tmp/${outfile}"
fi

if test ! -e "$infile"; then
  echo "ERROR: no input file specified"
  exit
fi

tool=$(grep "tests ===" "$infile" | tr -s ' ' | cut -d ' ' -f 2)

# Get the counts for tests that didn't work properly
skipped=$(grep -E -c '^UNRESOLVED|^UNTESTED|^UNSUPPORTED' "$infile")
if test x"${skipped}" = x; then
    skipped=0
fi

# The total of successful results are PASS and XFAIL
passes=$(grep -E -c '^PASS|XFAIL' "$infile")
if test x"${passes}" = x; then
    passes=0
fi

# The total of failed results are FAIL and XPASS
failures=$(grep -E -c '^FAIL|XPASS' "$infile")
if test x"${failures}" = x; then
    failures=0
fi

# Calculate the total number of test cases
total=$((passes + failures))
total=$((total + skipped))

cat <<EOF > "$outfile"
<?xml version="1.0"?>

<testsuites>
<testsuite name="DejaGnu" tests="${total}" failures="${failures}" skipped="${skipped}">

EOF

# Reduce the size of the file to be parsed to improve performance. Junit
# ignores sucessful test results, so we only grab the failures and test
# case problem results.
tmpfile="${infile}.tmp"
rm -f "$tmpfile"
grep -E 'PASS|XPASS|FAIL|XFAIL|UNTESTED|UNSUPPORTED|UNRESOLVED' "$infile" > "$tmpfile"

# strip non printable characters
sed -i 's/[^[:print:]]//g' "$tmpfile"

while read -r line
do
    result=${line%%:*}
    result=${result//[><;:\[\]^\\&?@\"\']/}
    name=${line#*: }
    name=${name//[><;:\[\]^\\&?@\"\']/}
    message=${line//[><;:\[\]^\\&?@\"\']/}

    echo "    <testcase name=\"${name}\" classname=\"${tool}-${result}\">" >> "$outfile"
    case "${result}" in
	UNSUPPORTED|UNTESTED|UNRESOLVED)
	    if test x"${message}" != x; then
		echo -n "        <skipped message=\"${message}\"/>" >> "$outfile"
	    else
		echo -n "        <skipped type=\"$result\"/>" >> "$outfile"
	    fi
	    ;;
	XPASS)
	    echo -n "        <failure message=\"$message\"/>" >> "$outfile"
	    ;;
	PASS|XFAIL)
	    # not a failure, expected
	    ;;
	*)
	    echo -n "        <failure message=\"$message\"/>" >> "$outfile"
    esac
    #echo "\"/>" >> "$outfile"

    echo "    </testcase>" >> "$outfile"
done < "$tmpfile"
rm -f "$tmpfile"

# Write the closing tag for the test results
echo "</testsuite>" >> "$outfile"
echo "</testsuites>" >> "$outfile"
