#!/bin/bash
if [ $# -lt 7 ]
then
	echo "Usage: ./test.sh [-c] mintasks maxtasks mingolden maxgolden minsats maxsats numrep"
else
	if [ $# -eq 7 ]
	then
		mintasks=$1
		maxtasks=$2
		mingolden=$3
		maxgolden=$4
		minsats=$5
		maxsats=$6
		numrep=$7
	elif [ $# -eq 8 ]
	then
		if [ $1 = "-c" ]
		then
			echo "Cleaning the blackboard..."
			echo "=========================="

			make clean

			echo " "
			echo "Compiling the files..."
			echo "======================"

			make
			echo " "
			mintasks=$2
			maxtasks=$3
			mingolden=$4
			maxgolden=$5
			minsats=$6
			maxsats=$7
			numrep=$8
		else
			echo "Usage: ./test.sh [-c] mintasks maxtasks mingolden maxgolden minsats maxsats numrep"
		fi
	else
		echo "Usage: ./test.sh [-c] mintasks maxtasks mingolden maxgolden minsats maxsats numrep"
	fi

	rm -fr "results_${mintasks}-${maxtasks}_${mingolden}-${maxgolden}_${minsats}-${maxsats}_${numrep}" 
	mkdir "results_${mintasks}-${maxtasks}_${mingolden}-${maxgolden}_${minsats}-${maxsats}_${numrep}"
	echo "Executing tests..."
	echo "=================="
	for i in `seq $minsats $maxsats`;
	do
		for j in `seq $mingolden $maxgolden`;
    	do
			for k in `seq $mintasks $maxtasks`;
			do 
				for m in `seq 1 $numrep`;
				do
					echo -n -e "\rSolving ${k} ${j} ${i} (${m})..."
					./sched.exe $k $j $i >> "results_${mintasks}-${maxtasks}_${mingolden}-${maxgolden}_${minsats}-${maxsats}_${numrep}/sched.txt";
				done
			done
		done
	done
	echo " "
	echo "Tests completed!!"
fi
