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
				make OMP_CONFIG="-DOPENMP_SCHEDULE_${omp_schedule} -DOPENMP_CHUNK_SIZE=${chunk_size} -DOPENMP_NUM_THREADS=${num_threads}"
				mv ${benchmark}-${PREFIX_BENCHMARK}.exe ${benchmark}-dataset-${size_of_data}-schedule-${omp_schedule}-chunk-${chunk_size}-threads-${num_threads}-${PREFIX_BENCHMARK}.exe
				for ((  i = 1 ;  i <= 10;  i++  ))
				do
					echo "Executing..."
					echo "Experiment '${EXPERIMENT}', execution ${i} of ${benchmark} with dataset: ${size_of_data}, schedule: ${omp_schedule}, chunk_size: ${chunk_size}, threads: ${num_threads} start at `date +'%d/%m/%Y-%T'`"
					echo "Experiment '${EXPERIMENT}', execution ${i} of ${benchmark} with dataset: ${size_of_data}, schedule: ${omp_schedule}, chunk_size: ${chunk_size}, threads: ${num_threads} start at `date +'%d/%m/%Y-%T'`" >> data-${benchmark}-dataset-${size_of_data}-schedule-${omp_schedule}-chunk-${chunk_size}-threads-${num_threads}-${PREFIX_BENCHMARK}-stderr.csv
					echo "exp = ${EXPERIMENT}, execution = ${i}, benchmark = ${benchmark}, size_of_data = ${size_of_data}, schedule = ${omp_schedule}, chunk_size = ${chunk_size}, num_threads = ${num_threads}," >> data-${benchmark}-dataset-${size_of_data}-schedule-${omp_schedule}-chunk-${chunk_size}-threads-${num_threads}-${PREFIX_BENCHMARK}.csv
					./${benchmark}-dataset-${size_of_data}-schedule-${omp_schedule}-chunk-${chunk_size}-threads-${num_threads}-${PREFIX_BENCHMARK}.exe ${size_of_data} >> data-${benchmark}-dataset-${size_of_data}-schedule-${omp_schedule}-chunk-${chunk_size}-threads-${num_threads}-${PREFIX_BENCHMARK}.csv 2>> data-${benchmark}-dataset-${size_of_data}-schedule-${omp_schedule}-chunk-${chunk_size}-threads-${num_threads}-${PREFIX_BENCHMARK}-stderr.csv
				done
			done
		done
	done
done
echo "End of tests at `date +'%d/%m/%Y-%T'`"
