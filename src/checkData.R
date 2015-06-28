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
