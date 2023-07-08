#!/bin/bash

failing_tests=()

for file in $(git diff --name-only tests)
do
	failing_tests+=$(echo $file | cut -d'/' -f2)
done

if [ ${#failing_tests[@]} -ne 0 ]
then
	echo -n "Failing tests:"

	for test in "${failing_tests[@]}"
	do
		echo -n " $test"
	done

	echo
	exit 1
fi

echo "All tests passing"
