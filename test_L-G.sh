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
			cd Scheduler/Scheduler
			make clean

			make
			cd ../..
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
	echo "[task_planner_globals]." > "${outdir}/swipl_execution.in"
	echo "[task_database]." >> "${outdir}/swipl_execution.in"
	echo "[load]." >> "${outdir}/swipl_execution.in"
	echo "drun." >> "${outdir}/swipl_execution.in"
	cd Task_Planner/
	maxtime=0
	summem=0
	for i in `seq 1 $sats`;
	do
		cp task_planner_globals_${i}.pl task_planner_globals.pl
		cp task_planner_globals_${i}.pl "../${outdir}/task_planner_globals_${i}.pl"
		/usr/bin/time -f "%M-%e" -o output_${i}.txt swipl < "../${outdir}/swipl_execution.in" > "../${outdir}/swipl_execution_${i}.out" 2> "../${outdir}/swipl_execution_${i}.err"
		temp=`cat output_${i}.txt | cut -d "-" -f 2`
		mem=`cat output_${i}.txt | cut -d "-" -f 1`
		rm output_${i}.txt
		comptemp=`echo $maxtime'>'$temp | bc -l`
		summem=`echo "$summem + $mem" | bc -l`
		if [ $comptemp -eq 0 ]
		then
			maxtime=$temp
		fi
		
		lastpath=`cat out/last_path.out`
		`cp -r out/${i} ../${outdir}`
		`cp out/${lastpath}* ../${outdir}/${i}`
	done
	cp task_database.pl "../${outdir}/task_database.pl"
	cd ../Scheduler/Scheduler/
	rm output.txt
	rm taskcount.txt
	/usr/bin/time -f "%M-%e" -o output.txt ./sched.exe ${tasks} ${maxgolden} ${sats} taskcount.txt > "../../${outdir}/sched.txt"
	temp=`cat output.txt | cut -d "-" -f 2`
	mem=`cat output.txt | cut -d "-" -f 1`
	tc=`cat taskcount.txt`
	totaltime=`echo "$temp + $maxtime" | bc -l`
	totalmem=`echo "$mem + $summem" | bc -l`
	echo "$totalmem - $mem - $summem - $totaltime - $temp - $maxtime - $tc"
fi
