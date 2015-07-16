#!/bin/bash

read -p "Generer un nouvel espace d'apprentissage ? [Y/n] " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]
then
	read -p "Faire un backup de res/ ? (les donn�es seront supprim�es) [Y/n] " -n 1 -r
	echo
	if [[ $REPLY =~ ^[Yy]$ ]]
	then
		echo "Backup en cours..."
		DATE=`date +%m_%d_%H_%M`
		DATE="backup_$DATE"
		mkdir $DATE
		cp -r res/* $DATE
		echo "Backup termin� !"
	fi
fi

echo "Construction des fichiers binaires..."
java -jar jforests.jar --cmd=generate-bin --ranking --folder . --file trainMART --file testMART --file validMART
echo "Construction des fichiers binaires termin�e !"
echo "Apprentissage en cours..."
java -jar jforests.jar --cmd=train --ranking --config-file dummy.properties --train-file trainMART.bin --validation-file validMART.bin --output-model output.txt
echo "Apprentissage termin� !"
echo "Prediction en cours..."
java -jar jforests.jar --cmd=predict --ranking --model-file output.txt --tree-type RegressionTree --test-file testMART.bin --output-file predictions.txt
echo "prediction.txt g�n�r� !"
echo "Predicton termin�e !"

rm jforests-*
rm output.txt
rm trainMART.bin
rm testMART.bin
rm validMART.bin
