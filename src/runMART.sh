#!/bin/bash

read -p "Generer un nouvel espace d'apprentissage ? [Y/n] " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]
then
	read -p "Faire un backup de res/ ? (les données seront supprimées) [Y/n] " -n 1 -r
	echo
	if [[ $REPLY =~ ^[Yy]$ ]]
	then
		DATE=`date +%m_%d_%H_%M`
		DATE="backup_$DATE"
		mkdir $DATE
		cp -r res/* $DATE
	fi
fi
