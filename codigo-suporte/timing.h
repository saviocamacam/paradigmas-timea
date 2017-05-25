#ifndef TIMING_H
#define TIMING_H
#include <stdint.h>
#include <stdbool.h>
#include <time.h>

/* stackoverflow clock_gettime tem precisão de nano 
   e gettimeofday de microsegundos.
*/
// Monitoring SEQUENTIAL version.
uint64_t seq_start, seq_stop;

// Monitoring OMP version.
uint64_t omp_start, omp_stop;

uint64_t get_time(){
 struct timespec spec;
 clock_gettime(CLOCK_MONOTONIC, &spec);
 return ((uint64_t)1e9) * spec.tv_sec + spec.tv_nsec;
}

#define HOOKOMP_TIMING_SEQ_START hookomp_timing_start(&seq_start)
#define HOOKOMP_TIMING_SEQ_STOP hookomp_timing_stop(&seq_stop)
#define HOOKOMP_TIMING_SEQ_PRINT hookomp_timing_print(seq_start,seq_stop)

// Global time omp code, with the possibility of device offloading.
#define HOOKOMP_TIMING_OMP_START hookomp_timing_start(&omp_start)
#define HOOKOMP_TIMING_OMP_STOP hookomp_timing_stop(&omp_stop)
#define HOOKOMP_TIMING_OMP_PRINT hookomp_timing_print(omp_start,omp_stop)

#define HOOKOMP_PRINT_TIME_RESULTS hookomp_print_time_results()

void hookomp_timing_start(uint64_t *_start){
	*_start = get_time();
}

void hookomp_timing_stop(uint64_t *_stop){
	*_stop = get_time();
}

void hookomp_timing_print(uint64_t tstart, uint64_t tstop){
	printf ("%Ld", tstop - tstart);
}

void hookomp_print_time_results(){
	fprintf(stdout, "ORIG = ");
  	HOOKOMP_TIMING_SEQ_PRINT;
  	fprintf(stdout, ", ");
  	fprintf(stdout, "OMP = ");
  	HOOKOMP_TIMING_OMP_PRINT;
  	fprintf(stdout, ", ");
  	fprintf(stdout, "\n");
}

#endif /* TIMING_H */
