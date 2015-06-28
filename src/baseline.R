#Fichier contenant les fonctions servant au calcul des baselines

source("import.R");
source("checkData.R");
source("func.R");

ratings = getRatings();
movies = getMovies();
user = getUsers();

MP = function(){

	yPrime = vector(mode = "integer", length = dim(movies)[1]);
	alpha = 2;
	yBarre = globalAverageRating();
	for(i in seq(dim(movies)[1])){
		j = movies[i,1];
		Nj = nbRat(j);
		yBarreJ = avg(j);
		num = yBarreJ * Nj + yBarre * alpha;
		den = Nj + alpha;
		yPrime[i] = num/den;
	}
	return(yPrime);
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

# Fonction retournant les k plus proches voisins d'un utilisateur
getNeighbors = function(user, k){

	#On initialise le vecteur de taille k a l'infini
	neighbors = matrix(nrow = k, ncol = 2);
	for(i in seq(k)){
		neighbors[i,1] = Inf;
		neighbors[i,2] = -1;
	}

	for(i in seq(dim(users)[1])){
		#On evite de comparer l'user avec lui meme
		if(i != user[1,1]){
			dist = euclideanDistance(user, users[i,]);
			#On conserve l'id de l'user compare
			dist[2] = i;
				#FINIR LA FONCTION BUBBLEADD ET CELLE-CI !!
			neighbors = bubbleAdd(neighbors, dist);	
		}
	}
	return(neighbors);

}

# Fonction calculant la distance euclidienne entre 2 utilisateurs
# >> discuter des ponderations
euclideanDistance = function(u1, u2){

	#On calcule la difference entre les ages des 2 users
	ageDist = sqrt( (u1[1,2]-u2[1,2])^2 );
	#On calcule la difference entre les sexes (ponderation variable, ici = 20)
	sexDist = (u1[1,3] == u2[1,3]) ? 0 : 20;
	#On calcule la difference entre les emplois (ponderation plus faible, ici -5/+5)
	jobDist = (u1[1,4] == u2[1,4]) ? -5 : 5;
	return(ageDist + sexDist + jobDist);
}
