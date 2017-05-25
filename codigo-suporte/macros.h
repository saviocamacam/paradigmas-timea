#ifndef MACROS_H
#define MACROS_H

#ifdef _OPENMP
#include <omp.h>
#else
#define omp_get_thread_num() 0
#define omp_get_num_threads() 1
#define omp_get_num_procs() (system("cat /proc/cpuinfo | grep 'processor' | wc -l"))
#endif

#if ! defined(OPENMP_SCHEDULE_RUNTIME) && ! defined(OPENMP_SCHEDULE_DYNAMIC) && ! defined(OPENMP_SCHEDULE_GUIDED)
#define OPENMP_SCHEDULE_RUNTIME
#endif

#ifndef OPENMP_CHUNK_SIZE
#define OPENMP_CHUNK_SIZE 32
#endif

#if defined(OPENMP_SCHEDULE_RUNTIME)
#define OPENMP_SCHEDULE_WITH_CHUNK runtime
#elif defined(OPENMP_SCHEDULE_DYNAMIC)
#define OPENMP_SCHEDULE_WITH_CHUNK dynamic, OPENMP_CHUNK_SIZE
#elif defined(OPENMP_SCHEDULE_GUIDED)
#define OPENMP_SCHEDULE_WITH_CHUNK guided, OPENMP_CHUNK_SIZE
#endif

#if ! defined(OPENMP_NUM_THREADS)
#define OPENMP_NUM_THREADS omp_get_num_procs()
#endif
#endif /* !MACROS */

