#include <stdio.h>
#include "macros.h"
#include "timing.h"
#include <stdlib.h>


float *h_a;
float *h_b;
float *h_c;

void init_array(int num_elementos)
{
  //fprintf(stdout, "Inicializando os arrays.\n");
  int i;
  for(i = 0; i < num_elementos ; i++)
  {
    h_a[i] = 0.5;
    h_b[i] = 0.5;
  }
}


int main(int argc, char *argv[]){

  int num_elementos = atoi(argv[1]);

  
  int i;

  h_a = malloc(num_elementos * sizeof(float));
  h_b = malloc(num_elementos * sizeof(float));
  h_c = malloc(num_elementos * sizeof(float));

  init_array(num_elementos);
  HOOKOMP_TIMING_SEQ_START;
  for(i = 0; i < num_elementos; i++)
    {
      h_c[i] = h_a[i] + h_b[i];
    }
  HOOKOMP_TIMING_SEQ_STOP;

	HOOKOMP_TIMING_OMP_START;
	#pragma omp parallel num_threads(OPENMP_NUM_THREADS)
  	{
  		#pragma omp for schedule(OPENMP_SCHEDULE_WITH_CHUNK)
      for(i = 0; i < num_elementos; i++)
      {
        h_c[i] = h_a[i] + h_b[i];
      }
  	}
  	HOOKOMP_TIMING_OMP_STOP;

  	fprintf(stdout, "version = OMP, num_threads = %d, N = %d, ", OPENMP_NUM_THREADS, num_elementos);
  	HOOKOMP_PRINT_TIME_RESULTS;

	return 0;
}