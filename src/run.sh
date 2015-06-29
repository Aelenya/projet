#!/bin/bash

#mpirun -np 1 R --slave CMD BATCH run.R
read -p "Faire un backup de res/ ? (les données seront supprimées) [Y/n] " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]
then
DATE=`date +%m_%d_%H%M`
DATE="backup_$DATE"
mkdir $DATE
cp -r res/* $DATE
fi

cd res
rm *
cd ..
mpirun -np 4 R --no-save --slave -f run.R
