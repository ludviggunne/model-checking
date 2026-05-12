#! /usr/bin/env bash

run_test() {
	local cfg="$1" spec="$2" expect="$3" actual

	printf "%-20s%-20s" "${cfg##*/}" "${spec##*/}"
	# echo -ne "${cfg##*/}\t${spec##*/}\t"

	if ./check "$cfg" "$spec"; then
		actual="GOOD"
	else
		actual="BAD"
	fi

	if [ ! "$expect" = "$actual" ]; then
		echo -e "\t> FAIL: expected $expect, got $actual"
	fi
}

while read -r cfg spec expect; do
	run_test "$cfg" "$spec" "$expect"
done <testcases.txt
