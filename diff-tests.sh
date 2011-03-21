#!/bin/bash

TEST_CLASSPATH="src/tool/ee/cyber/waebric/tests"
TEST_OUTPUT_CLASSPATH="src/tool/ee/cyber/waebric/testast"
TEST_OUTPUT_CLASSPATH2="src/tool/ee/cyber/waebric/testgen"

for i in {1..100}
do
	test $i -gt 9 && j=0$i || j=00$i && test $i -eq 100 && j=$i
	
	if [ -e "$TEST_OUTPUT_CLASSPATH/test$j.out" ]; then
		echo "Generating AST from test$j.wae..."
		./run-waebric-ast.sh $TEST_CLASSPATH/test$j.wae > tmp.txt
		echo "Diffing with existing test$j.out"
		diff -i -B tmp.txt $TEST_OUTPUT_CLASSPATH/test$j.out
			
	else
		echo "No AST test results found for test$j.wae"
	fi

	if [ -e "$TEST_OUTPUT_CLASSPATH2/test$j.html" ]; then
		echo "Generating HTML from test$j.wae..."
		./run-waebric-codegen.sh $TEST_CLASSPATH/test$j.wae > tmp.txt
		echo "Diffing with existing test$j.out"
		diff -i -B tmp.txt $TEST_OUTPUT_CLASSPATH2/test$j.html
	else
		echo "No generator test results found for test$j.wae"
	fi

done

rm tmp.txt

