#!/bin/bash
DATE=`date +%Y%m%dT%H%M%S`
outdir="out_M-B/results_${DATE}" 
rm -fr ${outdir}
mkdir ${outdir}
cd Market/
cp erl_execution.in "../${outdir}/erl_execution.in"
/usr/bin/time -f "%M" -o out/memory.txt erl < "erl_execution.in" > "../${outdir}/erl_execution.out"
mem=`cat out/memory.txt`
timing=`cat out/timing.out`
echo "$mem - $timing"
mv out/*.out "../${outdir}"
