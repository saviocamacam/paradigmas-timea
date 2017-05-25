#!/bin/bash -xe

benchmark=vectoradd
PREFIX_BENCHMARK=omp
EXPERIMENT=schedule-test

echo "Executing test for $benchmark, start at `date +'%d/%m/%Y-%T'`"

for size_of_data in 1048576 8388608; do #1048576 8388608
	for num_threads in 1 2 4 8; do #
		for omp_schedule in STATIC DYNAMIC GUIDED; do #
			for chunk_size in 16 32 64 128 256; do #
				echo "Compiling ${benchmark} with dataset: ${size_of_data}, schedule: ${omp_schedule}, chunk: ${chunk_size}, threads: ${num_threads}."
				echo "Executing..."
				echo "Experiment '${EXPERIMENT}', execution ${i} of ${benchmark} with dataset: ${size_of_data}, schedule: ${omp_schedule}, chunk_size: ${chunk_size}, threads: ${num_threads} start at `date +'%d/%m/%Y-%T'`"
				./csvize-experiments-results.py -i data-${benchmark}-dataset-${size_of_data}-schedule-${omp_schedule}-chunk-${chunk_size}-threads-${num_threads}-${PREFIX_BENCHMARK}.csv -o teste.csv
				cat teste.csv >> final.csv
				rm teste.csv
				
			done
		done
	done
done
echo "End of tests at `date +'%d/%m/%Y-%T'`"
