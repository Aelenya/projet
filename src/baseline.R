#Fichier contenant les fonctions servant au calcul des baselines

source("import.R");
ratings = getRatings();

MP = function(y){

	yPrime = vector();
	for(j in seq(dim(y)[1])){
		Nj = nbRat(j);
		yBarre = avg(j);
		alpha = 2;
		num = yBarre * Nj + yBarre * alpha;
		den = Nj + alpha;
		yPrime[j] = num/den;
	}
	return(nPrime);
}


UB = function(c,y){


}

# Fonction retournant la note moyenne des evaluations d'un film
avg = function(id){

	moy = 0;
	div = 0;
	for(i in seq(dim(ratings)[1])){
		rating = ratings[i,];
		if(rating[1,2] == id){
			moy = moy + rating[1,3];
			div = div + 1;
		}
	}
	moy = moy / div;
	return(moy);
}

# Fonction retournant le nombre d'evaluation pour un film
nbRat = function(id){

	nb = 0;
	for(i in seq(dim(ratings)[1])){
		rating = ratings[i,];
		if(rating[1,2] == id){
			nb = nb + 1;
		}
	}
	return(nb);
}
