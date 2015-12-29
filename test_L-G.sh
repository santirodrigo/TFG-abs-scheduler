#!/bin/bash
if [ $# -lt 3 ]
then
	echo "Usage: ./test_L-G.sh [-c] tasks maxgolden sats"
else
	if [ $# -eq 3 ]
	then
		tasks=$1
		maxgolden=$2
		sats=$3
	elif [ $# -eq 4 ]
	then
		if [ $1 = "-c" ]
		then
			echo "Cleaning the blackboard..."
			echo "=========================="

			cd Scheduler/Scheduler
			make clean

			echo " "
			echo "Compiling the files..."
			echo "======================"

			make
			cd ../..
			echo " "
			tasks=$2
			maxgolden=$3
			sats=$4
		else
			echo "Usage: ./test_L-G.sh [-c] tasks maxgolden sats"
		fi
	else
		echo "Usage: ./test_L-G.sh [-c] tasks maxgolden sats"
	fi

	DATE=`date +%Y%m%dT%H%M%S`
	outdir="out_L-G/results_${DATE}:${tasks}_${maxgolden}_${sats}" 
	rm -fr ${outdir}
	mkdir ${outdir}
	echo "Executing test..."
	echo "=================="
	echo "[task_planner_globals_1]." > "${outdir}/swipl_execution.in"
	echo "[task_database]." >> "${outdir}/swipl_execution.in"
	echo "[load]." >> "${outdir}/swipl_execution.in"
	echo "drun." >> "${outdir}/swipl_execution.in"
	for i in `seq 2 $sats`;
	do
		echo "[task_planner_globals_${i}]." >> "${outdir}/swipl_execution.in"
		echo "drun." >> "${outdir}/swipl_execution.in"
	done
	cd Task_Planner/ && swipl < "../${outdir}/swipl_execution.in" > "${outdir}/swipl_execution.out"
	Scheduler/Scheduler/sched.exe $tasks $maxgolden $sats > "${outdir}/sched.txt"
	echo " "
	echo "Tests completed!!"
fi
