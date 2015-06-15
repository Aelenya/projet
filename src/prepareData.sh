#!/bin/bash

# Petit script bash pour supprimer les apostrophes (') qui ne peuvent pas etre
# importees en R...
#
# Syntaxe pour modifications eventuelles de la commande sed : 
# sed -i "s/char a remplacer/char de remplacement/g" NOM_FICHIER


cd ../data/movies
for k in `seq 2`; do
	for file in u.*; do
		sed -i "s/'//g" $file
	done;
	if [ $k -eq 1 ]
		then cd ../users
	elif [ $k -eq 2 ]
		then cd learning
	fi;
done;
