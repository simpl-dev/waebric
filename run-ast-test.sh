#!/bin/bash

TEST_CLASSPATH="src/tests"
TEST_OUTPUT_CLASSPATH="src/testast"

echo "Input:"
cat $TEST_CLASSPATH/test$1.wae
echo ""
echo "Output:"
./run-waebric-ast.sh $TEST_CLASSPATH/test$1.wae

echo ""

if test -z "$2"
then
	echo "Output not saved..."
else
	if [ "$2" = "SAVE" ] 
	then
		echo "Saving output to $TEST_OUTPUT_CLASSPATH/test$1.out"
		./run-waebric-ast.sh $TEST_CLASSPATH/test$1.wae > $TEST_OUTPUT_CLASSPATH/test$1.out
	else
		echo "use SAVE as the second argument to save the test result"
	fi

fi



