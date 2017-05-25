BENCHMARK = vectoradd

CC = gcc

EXE = ${BENCHMARK}-omp.exe

SRC = $(BENCHMARK).c

CFLAGS = -fopenmp

.PHONY: all exe clean

all : exe

exe : clean $(EXE)

$(EXE) : $(SRC)
	$(CC) $(CFLAGS) $(OMP_CONFIG) $^ -o $@

clean :
	-rm -vf $(EXE)

info:
	@echo "Use:"
	@echo "make OMP_CONFIG=\"-DOPENMP_SCHEDULE_RUNTIME -DOPENMP_CHUNK_SIZE=64 -DOPENMP_NUM_THREADS=24\""
	@echo "make OMP_CONFIG=\"-DOPENMP_SCHEDULE_DYNAMIC -DOPENMP_CHUNK_SIZE=64 -DOPENMP_NUM_THREADS=24\""
	@echo "make OMP_CONFIG=\"-DOPENMP_SCHEDULE_GUIDED -DOPENMP_CHUNK_SIZE=64 -DOPENMP_NUM_THREADS=24\""

