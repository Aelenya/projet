# Fonctions permettant d'obtenir des informations importantes sur le set de donnees

source("import.R");
ratings = getRatings();
movies = getMovies();
users = getUsers();

ratingsPerUser = function(){

	nbUsers = dim(users)[1];
	nbMovies = dim(movies)[1];
	nbRatings = dim(ratings)[1];

	u = as.list(rep(0, nbUsers)); # liste pour obtenir le nb de rating par user
	moyU = as.list(rep(0, nbUsers));	# liste pour obtenir la moyenne des notes par user
	m = as.list(rep(0, nbMovies)); # liste pour obtenir le nombre de rating par film
	moyM = as.list(rep(0, nbMovies)); # liste obtenir la moyenne des notes par films
	r = as.list(rep(0, 5));	# 5 notes de 1 a 5

	for(i in seq(dim(ratings)[1])){
		rating = ratings[i,];
		u[[rating[[1]]]] = u[[rating[[1]]]] + 1;
		m[[rating[[2]]]] = m[[rating[[2]]]] + 1;
		r[[rating[[3]]]] = r[[rating[[3]]]] + 1;
		moyM[[rating[[2]]]] = moyM[[rating[[2]]]]+rating[[3]];
	}

	for(i in seq(length(m))){
		moyM[[i]] = moyM[[i]]/m[[i]];
	}

	plot(moyM,m,xlab="Moyenne des evaluations",ylab="Nombre d'evaluations",main="Moyenne des evaluations en fonction de leur nombre");
	abline( lm(as.numeric(m)~as.numeric(moyM)), col="red" );
}

globalAverageRating = function(){
	nbRatings = dim(ratings)[1];
	avg = 0;
	for(i in seq(nbRatings)){
		avg = avg + ratings[i,3];
	}
	avg = avg / nbRatings;
	return(avg);

}

ratingsPerFilm = function(){

	nbMovies = dim(movies)[1];
	nbRatings = dim(ratings)[1];
	retour = vector(mode = "integer", length = nbMovies);
	for(i in seq(nbMovies)){
		retour[i] = 0;
	}
	for(i in seq(nbRatings)){
		rating = ratings[i,];
		retour[rating[[2]]] = retour[rating[[2]]] + 1;
	}
	return(retour);
}

averageRatingPerFilm = function(nbRat){

	nbMovies = dim(movies)[1];
	nbRatings = dim(ratings)[1];
	retour = vector(mode = "integer", length = nbMovies);
	for(i in seq(nbMovies)){
		retour[i] = 0;
	}
	for(i in seq(nbRatings)){
		rating = ratings[i,];
		retour[rating[[2]]] = retour[rating[[2]]]+rating[[3]];
	}
	for(i in seq(length(retour))){
		retour[i] = retour[i]/nbRat[i];
	}
	return(retour);
}

#retourne le nombre moyen de label sur tous les films (chacun pouvant avoir plusieurs genre)
labelAverage.BASIC = function(){
	
	nbMovies = dim(movies)[1];
	somme = 0;
	for(i in seq(nbMovies)){
		movie = movies[i,];
		for(j in seq(6,dim(movie)[2])){
			if(movie[1,j] == 1){
				somme = somme + 1;
			}
		}
	}
	return(somme/nbMovies);
}
#Retourne le nombre moyen de label par entrees provenant de IMDb
labelAverage.IMDB = function(data){

	nbMovies = dim(movies)[1];
	somme = 0;
	for(i in seq(nbMovies)){
		movieId = as.integer(movies[i,1]);
		somme = somme + length(which(data[,2+movieId] > 0));
	}
	return(somme/nbMovies);

}
