#Fichier contenant les fonctions servant au calcul des baselines

source("import.R");
source("checkData.R");
source("func.R");

ratings = getRatings();
movies = getMovies();
users = getUsers();

MP = function(){

	yPrime = vector(mode = "integer", length = dim(movies)[1]);
	alpha = 2;
	yBarre = globalAverageRating();
	N = ratingsPerFilm();
	yBarreJPrime = averageRatingPerFilm(N);
	for(i in seq(dim(movies)[1])){
		j = movies[i,1];
		Nj = N[j];
		yBarreJ = yBarreJPrime[j];
		num = yBarreJ * Nj + yBarre * alpha;
		den = Nj + alpha;
		yPrime[i] = num/den;
	}
	yPrime = data.frame(matrix(unlist(yPrime),ncol = 1, byrow = TRUE));
	yPrime[,2:3] = 0;
	for(i in seq(dim(yPrime)[1])){
		yPrime[i,4] = i;
	}
	colnames(yPrime) = c("Like","Genre","Year","Movie.id");
	yPrime = yPrime[order(yPrime$Like, decreasing = TRUE),];
	return(yPrime);
}


UB = function(user){
	print(user[1,1]);
	nbMovies = dim(movies)[1];
	k = 15; #k-nearest neighbors
	yPrime = vector(mode = "integer", length = nbMovies);
	#On recupere les k plus proches autre utilisateurs
	nearest = getNeighbors(user, k);
	#Pour chaque film...
	for(j in seq(nbMovies)){
		somme = 0;
		div = 0;
		#Pour chaque voisin...
		for(n in seq(k)){
			#On trouve s'il a note le film, ratings[,1] = userId et
			#ratings[,2] = movieId
			r = which(ratings[,1] == nearest[n,2]);
			f = which(ratings[r,2] == j)				
			if(length(f) != 0){
				rat = r[f]; #id de la ratings du voisin qui a note ce film
				somme = somme + ratings[rat,3];
				div = div+1;
			}
		}
		somme = somme/div;
		yPrime[j] = somme;
	}
	#On met en forme le vecteur de retour pour correspondre au format des apprentissages
	yPrime = data.frame(matrix(unlist(yPrime),ncol = 1, byrow = TRUE));
	yPrime[,2:3] = 0;
	for(i in seq(dim(yPrime)[1])){
		yPrime[i,4] = i;
	}
	colnames(yPrime) = c("Like","Genre","Year","Movie.id");
	yPrime = yPrime[order(yPrime$Like, decreasing = TRUE),];
	yPrime = yPrime[-which(is.nan(yPrime[,1])),];
	return(yPrime);
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
