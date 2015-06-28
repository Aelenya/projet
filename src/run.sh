#!/bin/bash

#mpirun -np 1 R --slave CMD BATCH run.R
cd res
rm *
cd ..
mpirun -np 4 R --no-save --slave -f run.R
