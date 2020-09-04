#!/bin/bash
obeOutput="obe.output"
canOutput="cannonical.output"

#Clear out any .run .s and .o files from previous compilations/testing
make clean

######################## This section is the executable creation section ################################################
#if there are no files of the form test_gc_*.rkt, the for loop should not run
shopt -s nullglob
#Create the .run file for each gc test
for f in test_gc_*_obe.rkt 
do
	make  "$(basename "$f" .rkt).run"
done


function run_cannonical {
	racket -f "$1" > $2
}

###################### This section is the test execution section ####################################################
#run each gc test
for obeProgram in test_gc_*_obe.run
do	 
	cannonical="$(basename "$obeProgram" "_obe.run")_can.rkt" #get the filename of the corresponding cannonical racket file
	./$obeProgram > $obeOutput #execute the obe program
	run_cannonical $cannonical $canOutput #execute the cannonical program

	if cmp  "$obeOutput" "$canOutput"; then
		echo -e "\e[1;32m$obeProgram: Passed\e[0m"
	else
		# The files are different, display the differences
		diff --suppress-common-lines --side-by-side $obeOutput $canOutput
		echo -e "\e[1;31m$obeProgram: Failed\e[0m"
	fi
done
