#!/bin/bash
if [ $# -lt 12 ]
then
	echo "Usage: ./test_generator.sh\n\ttasks\n\ttasksize\n\tmingolden(escalar)\n\tmaxgolden\n\tsats\n\ttimelength\n\tenergy\n\tbadtasks [y/n]\n\ttaskdependence [y/n]\n\tgoldenfix [y/n]\n\tnumrep(escalar)\n\ttimescale(relation between prolog and erlang time)\n[interval input -> min:max]" # No ponemos de momento weights y/o cts ni priority_periods (lo dejamos fijo)
else
	if [ $# -eq 12 ]
	then
		error=0
		tasks=$1
		mintasks=`echo $tasks | cut -d ":" -f 1`
		maxtasks=`echo $tasks | cut -d ":" -f 2`
		if [ $maxtasks -lt $mintasks ]
		then
			echo "The tasks interval must be built as min:max"
			error=1
		fi
		if [ $maxtasks -lt 0 ]
		then
			echo "The number of tasks must be equal or higher than 0"
			error=1
		fi
		tasksize=$2
		mintasksize=`echo $tasksize | cut -d ":" -f 1`
		maxtasksize=`echo $tasksize | cut -d ":" -f 2`
		if [ $maxtasksize -lt $mintasksize ]
		then
			echo "The tasksize interval must be built as min:max"
			error=1
		fi
		if [ $maxtasksize -lt 0 ]
		then
			echo "The tasksize must be equal or higher than 0"
			error=1
		fi
		mingolden=$3
		if [ $mingolden -lt 0 ]
		then
			echo "The mingolden must be equal or higher than 0"
			error=1
		fi
		maxgolden=$4
		minmaxgolden=`echo $maxgolden | cut -d ":" -f 1`
		maxmaxgolden=`echo $maxgolden | cut -d ":" -f 2`
		if [ $maxmaxgolden -lt $minmaxgolden ]
		then
			echo "The maxgolden interval must be built as min:max"
			error=1
		fi
		if [ $maxmaxgolden -lt 0 ]
		then
			echo "The maxgolden must be equal or higher than 0"
			error=1
		fi
		if [ $minmaxgolden -lt $mingolden ]
		then
			echo "The mingolden must be lower than then minimum value for maxgolden"
			error=1
		fi
		sats=$5
		minsats=`echo $sats | cut -d ":" -f 1`
		maxsats=`echo $sats | cut -d ":" -f 2`
		if [ $maxsats -lt $minsats ]
		then
			echo "The sats interval must be built as min:max"
			error=1
		fi
		if [ $maxsats -lt 0 ]
		then
			echo "The number of sats must be equal or higher than 0"
			error=1
		fi		
		timelength=$6
		mintimelength=`echo $timelength | cut -d ":" -f 1`
		maxtimelength=`echo $timelength | cut -d ":" -f 2`
		if [ $maxtimelength -lt $mintimelength ]
		then
			echo "The timelength interval must be built as min:max"
			error=1
		fi
		if [ $maxtimelength -lt 0 ]
		then
			echo "The timelength must be equal or higher than 0"
			error=1
		fi		
		energy=$7
		minenergy=`echo $energy | cut -d ":" -f 1`
		maxenergy=`echo $energy | cut -d ":" -f 2`
		if [ $maxenergy -lt $minenergy ]
		then
			echo "The energy interval must be built as min:max"
			error=1
		fi
		if [ $maxenergy -lt 0 ]
		then
			echo "The energy must be equal or higher than 0"
			error=1
		fi
		badtasks=$8
		taskdependence=$9
		goldenfix=${10}
		numrep=${11}
		timescale=${12}
	else
		echo "Usage: ./test_generator.sh\n\ttasks\n\ttasksize\n\tmingolden(escalar)\n\tmaxgolden\n\tsats\n\ttimelength\n\tenergy\n\tbadtasks [y/n]\n\ttaskdependence [y/n]\n\tgoldenfix [y/n]\n\tnumrep(escalar)\n\ttimescale(relation between prolog and erlang time)\n[interval input -> min:max]" # No ponemos de momento weights y/o cts ni priority_periods (lo dejamos fijo)
	fi
	
	if [ $error -eq 1 ]
	then
		echo "Couldn't read the input"
	else
		DATE=`date +%Y%m%dT%H%M%S`
		outdir="test_generator/generated_${DATE}" 
		rm -fr ${outdir}
		mkdir ${outdir}
		echo "Creating tests..."
		echo "=================="
		for z in `seq 1 $numrep`;
		do
			echo -e "nsats\tgolden\tntasks\twindow\t- timingLG - timingMB" > "${outdir}/results_${z}.txt"
		done
		echo "$minsats $maxsats $minmaxgolden $maxmaxgolden $mintasks $maxtasks $mintimelength $maxtimelength $numrep"
		for i in `seq $minsats $maxsats`;
		do
			for j in `seq $minmaxgolden $maxmaxgolden`;
			do
				for k in `seq $mintasks $maxtasks`;
				do 
					for l in `seq $mintimelength $maxtimelength`;
					do
						for z in `seq 1 $numrep`;
						do
							echo -n -e "\rCreating ${i} ${j} ${k} ${l}..."
							echo "MyBeginTime = now()." > "${outdir}/erl_execution_${i}_${j}_${k}_${l}.in"
							echo "{A1, A2, A3} = MyBeginTime." >> "${outdir}/erl_execution_${i}_${j}_${k}_${l}.in"

							echo "MissionStartTime = 0." >> "${outdir}/erl_execution_${i}_${j}_${k}_${l}.in"
							echo "MissionEndTime = ${l}." >> "${outdir}/erl_execution_${i}_${j}_${k}_${l}.in"
							echo "NSats = ${i}." >> "${outdir}/erl_execution_${i}_${j}_${k}_${l}.in"
							echo -n "NameSats = [" >> "${outdir}/erl_execution_${i}_${j}_${k}_${l}.in"
							for m in `seq 1 $i`;
							do
								awk -v m="$m" 'BEGIN{printf "%c",m+96}' >> "${outdir}/erl_execution_${i}_${j}_${k}_${l}.in"
								if [ ! $m -eq $i ]
								then
									echo -n ", " >> "${outdir}/erl_execution_${i}_${j}_${k}_${l}.in"
								fi
							done
							echo "]." >> "${outdir}/erl_execution_${i}_${j}_${k}_${l}.in"
							echo -n "Energies = [" >> "${outdir}/erl_execution_${i}_${j}_${k}_${l}.in"
							# Para cada satélite construimos sus valores
							for m in `seq 1 $i`;
							do
								energy=`shuf -i $minenergy-$maxenergy -n 1 | tr -d '\n'`
								energy_erl=`expr $energy \* $timescale`
								echo -n "$energy_erl" >> "${outdir}/erl_execution_${i}_${j}_${k}_${l}.in"
								if [ ! $m -eq $i ]
								then
									echo -n ", " >> "${outdir}/erl_execution_${i}_${j}_${k}_${l}.in"
								fi
								# Aprovechamos y hacemos aquí el task_planner_globals completo
								echo ":- module(task_planner_globals_${m}, [" > "${outdir}/task_planner_globals_${m}_${i}_${j}_${k}_${l}.pl"
								echo -e "\tscheduler_param/2," >> "${outdir}/task_planner_globals_${m}_${i}_${j}_${k}_${l}.pl"
		                        echo -e "\tresource_options/2," >> "${outdir}/task_planner_globals_${m}_${i}_${j}_${k}_${l}.pl"
		                        echo -e "\tresource_capacity/2" >> "${outdir}/task_planner_globals_${m}_${i}_${j}_${k}_${l}.pl"
		                        echo -e "\t])." >> "${outdir}/task_planner_globals_${m}_${i}_${j}_${k}_${l}.pl"

								echo "/** <module> Global Parameters and Resources" >> "${outdir}/task_planner_globals_${m}_${i}_${j}_${k}_${l}.pl"

								echo "This module includes the Task Scheduler parameters which configure the reduction of task domains and"  >> "${outdir}/task_planner_globals_${m}_${i}_${j}_${k}_${l}.pl"
								echo "the solver algorithm. It also contains all the available resources, listed in facts, their options" >> "${outdir}/task_planner_globals_${m}_${i}_${j}_${k}_${l}.pl"
								echo "and their capacities." >> "${outdir}/task_planner_globals_${m}_${i}_${j}_${k}_${l}.pl"
								echo "" >> "${outdir}/task_planner_globals_${m}_${i}_${j}_${k}_${l}.pl"
								echo "@author Santiago Rodrigo Muñoz" >> "${outdir}/task_planner_globals_${m}_${i}_${j}_${k}_${l}.pl"
								echo "*/" >> "${outdir}/task_planner_globals_${m}_${i}_${j}_${k}_${l}.pl"
								echo "" >> "${outdir}/task_planner_globals_${m}_${i}_${j}_${k}_${l}.pl"

								echo "%%  scheduler_param(+Param,-Value)" >> "${outdir}/task_planner_globals_${m}_${i}_${j}_${k}_${l}.pl"
								echo "%" >> "${outdir}/task_planner_globals_${m}_${i}_${j}_${k}_${l}.pl"
								echo "%" >> "${outdir}/task_planner_globals_${m}_${i}_${j}_${k}_${l}.pl"

								echo "scheduler_param(satellite_id		, ${m})." >> "${outdir}/task_planner_globals_${m}_${i}_${j}_${k}_${l}.pl"
								echo "scheduler_param(mission_start       , -1000)." >> "${outdir}/task_planner_globals_${m}_${i}_${j}_${k}_${l}.pl"
								echo "scheduler_param(time_start          , 0)." >> "${outdir}/task_planner_globals_${m}_${i}_${j}_${k}_${l}.pl"
								echo "scheduler_param(time_end            , ${l})." >> "${outdir}/task_planner_globals_${m}_${i}_${j}_${k}_${l}.pl"
								echo "scheduler_param(priority_periods	, 10). % N_s"  >> "${outdir}/task_planner_globals_${m}_${i}_${j}_${k}_${l}.pl" # De momento, fijo...
								echo "scheduler_param(weights,[1,1,1,1,1]). % Weights for F [Wc,Wg,Wu,We,Wd]),"  >> "${outdir}/task_planner_globals_${m}_${i}_${j}_${k}_${l}.pl" # De momento, fijo
								echo "scheduler_param(domain_similarity   , 0.70)." >> "${outdir}/task_planner_globals_${m}_${i}_${j}_${k}_${l}.pl"
								echo "scheduler_param(minimize_overlapping, false)." >> "${outdir}/task_planner_globals_${m}_${i}_${j}_${k}_${l}.pl"
								echo "scheduler_param(mo_allow_permutation, false)." >> "${outdir}/task_planner_globals_${m}_${i}_${j}_${k}_${l}.pl"
								echo "scheduler_param(mo_max_group_members, 10)." >> "${outdir}/task_planner_globals_${m}_${i}_${j}_${k}_${l}.pl"
								echo "scheduler_param(mo_once             , true)." >> "${outdir}/task_planner_globals_${m}_${i}_${j}_${k}_${l}.pl"
								echo "scheduler_param(labeling_options    , [ffc,bisect]). %random_value(376))." >> "${outdir}/task_planner_globals_${m}_${i}_${j}_${k}_${l}.pl"
								echo "scheduler_param(algorithm_timeout   , 3600)." >> "${outdir}/task_planner_globals_${m}_${i}_${j}_${k}_${l}.pl"
								
								if [ $goldenfix = "n" ]
								then
									delta=`shuf -i $mingolden-${j} -n 1 | tr -d '\n'` ### DELTA
								else
									delta=${j}
								fi
								
								echo "scheduler_param(delta_solutions		, $delta)." >> "${outdir}/task_planner_globals_${m}_${i}_${j}_${k}_${l}.pl"
								echo "scheduler_param(pred_temperature    , 'predictor_temperature.out'). % SRM: NEEDED" >> "${outdir}/task_planner_globals_${m}_${i}_${j}_${k}_${l}.pl"
								echo "scheduler_param(pred_radiation      , 'predictor_radiation.out'). % SRM: NEEDED" >> "${outdir}/task_planner_globals_${m}_${i}_${j}_${k}_${l}.pl"
								echo "scheduler_param(orbit_propagator    , 'orbit_propagator.out'). % SRM: NEEDED" >> "${outdir}/task_planner_globals_${m}_${i}_${j}_${k}_${l}.pl"

								echo "" >> "${outdir}/task_planner_globals_${m}_${i}_${j}_${k}_${l}.pl"
								echo "%%  resource_options(+Rname, -Options)" >> "${outdir}/task_planner_globals_${m}_${i}_${j}_${k}_${l}.pl"
								echo "%" >> "${outdir}/task_planner_globals_${m}_${i}_${j}_${k}_${l}.pl"

								echo "resource_options(simultaneity,[cumulative(false)])." >> "${outdir}/task_planner_globals_${m}_${i}_${j}_${k}_${l}.pl"
								echo "resource_options(power     ,[cumulative(true)])." >> "${outdir}/task_planner_globals_${m}_${i}_${j}_${k}_${l}.pl"
								echo "" >> "${outdir}/task_planner_globals_${m}_${i}_${j}_${k}_${l}.pl"
								echo "%%  resource_capacity(+Rname, -Capacity)" >> "${outdir}/task_planner_globals_${m}_${i}_${j}_${k}_${l}.pl"
								echo "%" >> "${outdir}/task_planner_globals_${m}_${i}_${j}_${k}_${l}.pl"
								timend=`expr $l - 1`
								doubletimend=`expr $l \* 2`
								echo "resource_capacity(power     ,[   $energy-$timend, 	% Definir hasta el time_end menos 1" >> "${outdir}/task_planner_globals_${m}_${i}_${j}_${k}_${l}.pl"
								echo -e "\t100000-$doubletimend]). 	% Recursos infinitos" >> "${outdir}/task_planner_globals_${m}_${i}_${j}_${k}_${l}.pl"
								echo "resource_capacity(simultaneity,[     1-$timend," >> "${outdir}/task_planner_globals_${m}_${i}_${j}_${k}_${l}.pl"
								echo -e "\t$k-$doubletimend]). % ntasks" >> "${outdir}/task_planner_globals_${m}_${i}_${j}_${k}_${l}.pl"
							done
							echo "]." >> "${outdir}/erl_execution_${i}_${j}_${k}_${l}.in"
							echo "Cts = [1,1,1]." >> "${outdir}/erl_execution_${i}_${j}_${k}_${l}.in"

							# Ahora vamos a las tareas: primero preparamos task_database.pl...
							echo ":- module(task_database, [" > "${outdir}/task_database_${i}_${j}_${k}_${l}.pl"
		                    echo -e "\ttask/1," >> "${outdir}/task_database_${i}_${j}_${k}_${l}.pl"
		                    echo -e "\ttask_priority/2," >> "${outdir}/task_database_${i}_${j}_${k}_${l}.pl"
		                    echo -e "\ttask_timing/7," >> "${outdir}/task_database_${i}_${j}_${k}_${l}.pl"
		                    echo -e "\ttask_positioning/3," >> "${outdir}/task_database_${i}_${j}_${k}_${l}.pl"
		                    echo -e "\ttask_static_constraints/2," >> "${outdir}/task_database_${i}_${j}_${k}_${l}.pl"
		                    echo -e "\ttask_dynamic_resources/2" >> "${outdir}/task_database_${i}_${j}_${k}_${l}.pl"
							echo -e "\t])." >> "${outdir}/task_database_${i}_${j}_${k}_${l}.pl"

							echo ":- discontiguous task/1, task_priority/2, task_timing/6, task_positioning/3, task_static_constraints/2, task_dynamic_resources/2." >> "${outdir}/task_database_${i}_${j}_${k}_${l}.pl"

							# Y ahora para cada tarea le damos sus valores
							if [ $badtasks = "n" ]
							then
								for m in `seq 1 $k`;
								do
									duration=`shuf -i $mintasksize-$maxtasksize -n 1 | tr -d '\n'`
									mindead=`expr $duration + 1`
									deadline=`shuf -i $mindead-$l -n 1 | tr -d '\n'`
									maxstart=`expr $deadline - $duration - 1`
									start=`shuf -i 0-$maxstart -n 1 | tr -d '\n'`
									echo "$start:$duration:$deadline" >> temp.txt
								done
								timesall=`cat temp.txt | sort -n`
								rm temp.txt
							fi
							startall=`shuf -i 0-$l -n $k | sort -n | tr "\n" " "`
							for m in `seq 1 $k`;
							do
								if [ $badtasks = "y" ]
								then
									start=`echo $startall | cut -d " " -f $m`
									duration=`shuf -i $mintasksize-$maxtasksize -n 1 | tr -d '\n'`
									mindead=`expr $start + $duration + 1`
									if [ $l -lt $mindead ]
									then
										deadline=$mindead
									else
										deadline=`shuf -i $mindead-$l -n 1 | tr -d '\n'`
									fi
								else
									times=`echo $timesall | tr " " "-" | cut -d "-" -f $m`
									start=`echo $times | cut -d ":" -f 1`
									duration=`echo $times | cut -d ":" -f 2`
									deadline=`echo $times | cut -d ":" -f 3`
								fi
								if [ $taskdependence = "y" ]
								then
									previous=`expr $m - 1`	
								else
									previous=0
								fi
								dep=`shuf -i 0-$previous -n 1 | tr -d '\n'`
								# Primero task_database.pl...
								echo -e "task(${m})." >> "${outdir}/task_database_${i}_${j}_${k}_${l}.pl"
								echo -e "task_priority(${m},1)." >> "${outdir}/task_database_${i}_${j}_${k}_${l}.pl"
								echo -e "task_timing(${m}, period(0,0,15,0), duration($duration), init_delay($start), infinity, 0.000000, deadline($deadline))." >> "${outdir}/task_database_${i}_${j}_${k}_${l}.pl"
								echo -e "task_positioning(${m}, positions([]), 5)." >> "${outdir}/task_database_${i}_${j}_${k}_${l}.pl"
								echo -e "task_static_constraints(${m},resources([" >> "${outdir}/task_database_${i}_${j}_${k}_${l}.pl"
								echo -e "\ttemperature_range(10,300)," >> "${outdir}/task_database_${i}_${j}_${k}_${l}.pl"
								echo -e "\tradiation_range(0,1000)" >> "${outdir}/task_database_${i}_${j}_${k}_${l}.pl"
								echo -e "\t]))." >> "${outdir}/task_database_${i}_${j}_${k}_${l}.pl"
								echo -e "task_dynamic_resources(${m},resources([" >> "${outdir}/task_database_${i}_${j}_${k}_${l}.pl"
								echo -e "\tsimultaneity([1-$duration])," >> "${outdir}/task_database_${i}_${j}_${k}_${l}.pl"
								echo -e "\tpower([1-$duration])" >> "${outdir}/task_database_${i}_${j}_${k}_${l}.pl"
								echo -e "\t]))." >> "${outdir}/task_database_${i}_${j}_${k}_${l}.pl"
							
								# Y luego el erlang input
								echo "Task${m}Start = $start." >> "${outdir}/erl_execution_${i}_${j}_${k}_${l}.in"
								echo "Task${m}Deadline = $deadline." >> "${outdir}/erl_execution_${i}_${j}_${k}_${l}.in"
								echo "Task${m}Duration = $duration." >> "${outdir}/erl_execution_${i}_${j}_${k}_${l}.in"
								echo "Task${m}Dep = $dep." >> "${outdir}/erl_execution_${i}_${j}_${k}_${l}.in"
								echo "Task${m}StartS = trunc(Task${m}Start * $timescale / 1000)." >> "${outdir}/erl_execution_${i}_${j}_${k}_${l}.in"
								echo "Task${m}StartuS = (Task${m}Start * $timescale) rem 1000." >> "${outdir}/erl_execution_${i}_${j}_${k}_${l}.in"
								echo "Task${m}BeginTime = {A1, A2+Task${m}StartS, A3+Task${m}StartuS}." >> "${outdir}/erl_execution_${i}_${j}_${k}_${l}.in"
							done

							echo "market:start(NSats," >> "${outdir}/erl_execution_${i}_${j}_${k}_${l}.in"
							echo "NameSats," >> "${outdir}/erl_execution_${i}_${j}_${k}_${l}.in"
							echo "MyBeginTime," >> "${outdir}/erl_execution_${i}_${j}_${k}_${l}.in"
							echo -n "[" >> "${outdir}/erl_execution_${i}_${j}_${k}_${l}.in"
							for m in `seq 1 $i`;
							do
								echo -n "market:random_sphere_point(7350)" >> "${outdir}/erl_execution_${i}_${j}_${k}_${l}.in"
								if [ ! $m -eq $i ]
								then
									echo "," >> "${outdir}/erl_execution_${i}_${j}_${k}_${l}.in"
								fi
							done
							echo "]," >> "${outdir}/erl_execution_${i}_${j}_${k}_${l}.in"
							echo "Energies," >> "${outdir}/erl_execution_${i}_${j}_${k}_${l}.in"
							echo -n "[" >> "${outdir}/erl_execution_${i}_${j}_${k}_${l}.in"
							for m in `seq 1 $i`;
							do
								echo -n "Cts" >> "${outdir}/erl_execution_${i}_${j}_${k}_${l}.in"
								if [ ! $m -eq $i ]
								then
									echo -n ", " >> "${outdir}/erl_execution_${i}_${j}_${k}_${l}.in"
								fi
							done
							echo "])." >> "${outdir}/erl_execution_${i}_${j}_${k}_${l}.in"
							for m in `seq 1 $k`;
							do
								echo "timer:send_after(Task${m}Start*$timescale, a, {task, Task${m}BeginTime, [${m}, $timescale*Task${m}Duration, (Task${m}Deadline-Task${m}Start+MissionStartTime)*$timescale, Task${m}Dep]})." >> "${outdir}/erl_execution_${i}_${j}_${k}_${l}.in"
							done

							echo "timer:apply_after($timescale*MissionEndTime,market,stop,[NameSats])." >> "${outdir}/erl_execution_${i}_${j}_${k}_${l}.in"
							echo "timer:sleep($timescale*MissionEndTime+2500)." >> "${outdir}/erl_execution_${i}_${j}_${k}_${l}.in"
						
							# Ahora toca copiar al directorio que toca y ejecutar...
						
							cp "${outdir}/erl_execution_${i}_${j}_${k}_${l}.in" "Market/erl_execution.in"
							timingMB=`./test_M-B.sh`
							for m in `seq 1 $i`;
							do
								cp "${outdir}/task_planner_globals_${m}_${i}_${j}_${k}_${l}.pl" "Task_Planner/task_planner_globals_${m}.pl"
							done
							cp "${outdir}/task_database_${i}_${j}_${k}_${l}.pl" "Task_Planner/task_database.pl"
							timingLG=`./test_L-G.sh ${k} ${j} ${i}`
							echo -e "$i\t$j\t$k\t$l\t- $timingLG - $timingMB" >> "${outdir}/results_${z}.txt"
						done
					done
				done
			done
		done
		echo " "
		echo "Tests completed!!"
	fi
fi
