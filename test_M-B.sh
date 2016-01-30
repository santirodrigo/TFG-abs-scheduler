#!/bin/bash
DATE=`date +%Y%m%dT%H%M%S`
outdir="out_M-B/results_${DATE}" 
rm -fr ${outdir}
mkdir ${outdir}
cd Market/
cp erl_execution.in "../${outdir}/erl_execution.in"
/usr/bin/time -f "%M" -o out/memory.txt erl < "erl_execution.in" > "../${outdir}/erl_execution.out"
mem=`cat out/memory.txt`
timing=`cat out/timing.out | cut -d "-" -f 1`
tc=`cat out/timing.out | cut -d "-" -f 2`
timesec=`echo "$timing / 1000" | bc -l`
echo "$mem - $timesec - $tc"
mv out/*.out "../${outdir}"
