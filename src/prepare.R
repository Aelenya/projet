# Fichier contenant les fonctions necessaires pour le point 2 'Preliminaries'

library(e1071);
source("import.R");
source("func.R");
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
	for(i in seq(n)){
		for(j in seq(m)){
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
	for(i in seq(dim(ratings)[1])){
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
	for(i in seq(6,dim(genres)[1])){
		if(movie[1,i] == 1){
		movieGenres[k] = genres[i-5,1];
		}
	}


	var = subset(movie, select = c("empty","Action","Adventure","Animation","Childrens","Comedy","Crime","Documentary","Drama","Fantasy","Film.Noir","Horror","Musical","Mystery","Romance","Sci.Fi","Thriller","War","Western"));
	#res = svm(var, data = movie,  movie$movie.id);
	#res = svm(Action~.,data=getMovies();
	return(res);
}

#item cold start sans elements ajoutes de IMDb
itemCold.BASIC = function(user)
{
	userId = user[1,1];
	userRatings = list();
	k = 1;
	for(i in seq(dim(ratings)[1])){
		if(ratings[i,1] == userId){
			userRatings[[k]] = ratings[i,c(2,3)];
			k = k + 1;
		}
	}
	movies = getMovies();
	nbMovies = dim(movies)[1];
	# on tire aleatoirement 1/3 des films comme testData
	testId = sample(nbMovies, trunc(nbMovies/3));
	testMovies = movies[testId,];
	# les 2/3 restants seront donc les trainData
	trainMovies = movies[-testId,];
	
	#Construction des train data
	trainData = list();
	A = 1;

	for(i in seq(dim(trainMovies)[1])){
		for(j in seq(length(userRatings))){
			#Si le film a ete note par l'utilisateur
			if(trainMovies[i,1] == userRatings[[j]][1,1]){
				#On recupere les infos du film
				movie = movies[trainMovies[i,1],];
				#Si note > 3 : il a aime (1), sinon non (0), recherche svm
				y = userRatings[[j]][1,2] > 3 ? 1 : 0;

				#On recupere les genres du film (classe svm)
				movieGenres = list();
        			k = 1;
	      			for(i in seq(6,dim(movie)[2])){
                			if(movie[1,i] == 1){
		  				movieGenres[[k]] = genres[i-5,1];
		  				k = k+1;
                			}
        			}
				#On recupere la date du film (classe svm)
				date = as.numeric(strsplit(as.character(movie[1,3]),"-")[[1]][3]);
				#On ajoute aux trainData la note,le genre et la date
				for(i in seq(1,length(movieGenres))){
					#On extrait le nom du genre -syntaxe made in R...-
					g = levels(movieGenres[[i]])[(levels(movieGenres[[i]]) == movieGenres[[i]])];
					#On s'en sert pour obtenir l'id du genre
					id = which(genres[,1] == g);
					#On n'ajoute qu'une fois la date [cf 1.]
					if(i == 1){
						trainData[[A]] = c(y,id,date);
					}
					else{
						trainData[[A]] = c(y,id,NA);
					}
					A = A + 1;
				}
				#On peut break pour ce film et passer au suivant
				break;
			} 
		}
	}
	trainData = data.frame(matrix(unlist(trainData), ncol = 3, byrow = TRUE));
	colnames(trainData) = c("Like","Genre","Year");
return(trainData);
	# On conserve les GENRES des films
	#testData = subset(testData, select = c("empty","Action","Adventure","Animation","Childrens","Comedy","Crime","Documentary","Drama","Fantasy","Film.Noir","Horror","Musical","Mystery","Romance","Sci.Fi","Thriller","War","Western"));
	#trainData = subset(trainData, select = c("empty","Action","Adventure","Animation","Childrens","Comedy","Crime","Documentary","Drama","Fantasy","Film.Noir","Horror","Musical","Mystery","Romance","Sci.Fi","Thriller","War","Western"));

	# on applique un svm sur les trainData
	result = svm(Action~., data = trainData);
	prediction = predict(result, testData[,-2]);
	res = table(pred = prediction, true = testData[,2]);
	return(res);
}
