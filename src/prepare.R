# Fichier contenant les fonctions necessaires pour le point 2 'Preliminaries'

source("import.R");

ratings = getRatings();
genres = getGenres();

getY = function(){

	#On recupere la matrice d'utilisateur C de taille n x d
	C = getUsers();
	n = dim(C)[1];
	d = dim(C)[2];

	#On recupere la matrice d'item D de taille m x l
	D = getMovies();
	m = dim(D)[1];
	l = dim(D)[2];

	#Pour chaque utilisateurs parmi n, on doit construire la matrice Y de taille n x m
	#contenant le produit cartesien c x d
	Y = array(list(), c(n,m));
	for(i = seq(n)){
		for(j = seq(m)){
			c = C[i,];
			d = D[j,];
			Y[i,j] = f(c,d);
		}
	}
	return(Y);
}

#Fonction f qui renvoie la valeur y en effectuant le produit cartesien c x d
f = function(user, movie){
	
	#On obtient l'id de l'utilisateur
	userId = user[1,1];
	
	#On obtient toutes les notes qu'il a attribue
	userRatings = list();
	k = 1;
	for(i = seq(dim(ratings)[1])){
		if(ratings[i,1] == userId){
			#on ne conserve que l'id du film et la note donnee
			userRatings[k] = ratings[i,c(2,3)];
			k = k + 1;
		}
	}
	
	#On obtient l'id du film
	movieId = movie[1,1];
	
	#On obtient le(s) genre(s) du film
	movieGenres = c();
	k = 1;
	for(i = seq(6,dim(genres)[1])){
		if(movie[1,i] == 1){
			movieGenres[k] = genres[i-5,1];
		}
	}
}
