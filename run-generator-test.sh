#!/bin/bash

TEST_CLASSPATH="src/tests"
TEST_OUTPUT_CLASSPATH="src/testgen"

echo "Input:"
cat $TEST_CLASSPATH/test$1.wae
echo ""
echo "Output:"
./run-waebric-codegen.sh $TEST_CLASSPATH/test$1.wae

echo ""

if test -z "$2"
then
	echo "Output not saved..."
else
	if [ "$2" = "SAVE" ] 
	then
		echo "Saving output to $TEST_OUTPUT_CLASSPATH/test$1.html"
		./run-waebric-codegen.sh $TEST_CLASSPATH/test$1.wae > $TEST_OUTPUT_CLASSPATH/test$1.html
	else
		echo "use SAVE as the second argument to save the test result"
	fi

fi



