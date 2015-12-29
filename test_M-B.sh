#!/bin/bash
	DATE=`date +%Y%m%dT%H%M%S`
	outdir="out_M-B/results_${DATE}:${tasks}_${maxgolden}_${sats}" 
	rm -fr ${outdir}
	mkdir ${outdir}
	echo "Executing test..."
	echo "=================="
	cd Market/
	cp erl_execution.in "../${outdir}/erl_execution.in"
	erl < "erl_execution.in" > "../${outdir}/erl_execution.out"
	mv out/*.out "../${outdir}"
	echo " "
	echo "Tests completed!!"
fi
