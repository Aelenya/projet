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

#fonction initialisant les start (gere le sel + construction des donnees)
init = function(){
	movies = getMovies();
	nbMovies = dim(movies)[1];
	testId = sample(nbMovies, trunc(nbMovies/3));
	return(testId);
}

#item cold start sans elements ajoutes de IMDb
itemCold.BASIC = function(user, testId)
{
print(user);
	#userId = user[1,1];
	userId = user;
	userRatings = list();
	k = 1;
	for(i in seq(dim(ratings)[1])){
		if(ratings[i,1] == userId){
			userRatings[[k]] = ratings[i,c(2,3)];
			k = k + 1;
		}
	}
	movies = getMovies();
	# on tire aleatoirement 1/3 des films comme testData
	testMovies = movies[testId,];
	# les 2/3 restants seront donc les trainData
	trainMovies = movies[-testId,];
	
	#Construction des test data (cf commentaire des train data, plus complet)
	testData = list();
	A = 1;
	for(i in seq(dim(testMovies)[1])){
		movie = movies[testMovies[i,1],];
		movieGenres = list();
		k = 1;
		for(x in seq(6,dim(movie)[2])){
			if(movie[1,x] == 1){
				movieGenres[[k]] = genres[x-5,1];
				k = k+1;
			}
		}
		date = as.numeric(strsplit(as.character(movie[1,3]),"-")[[1]][3]);
		for(x in seq(1,length(movieGenres))){
			g = levels(movieGenres[[x]])[(levels(movieGenres[[x]]) == movieGenres[[x]])];
			id = which(genres[,1] == g);
			#On met la premiere colonne a 0 (colonne a "predire")
			if(x == 1){
				testData[[A]] = c(0,id,date);
			}
			else{
				testData[[A]] = c(0,id,NA);
			}
			A = A+1;
		}
	}

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
				#y = userRatings[[j]][1,2] > 3 ? 1 : 0;
				y = userRatings[[j]][1,2];

				#On recupere les genres du film (classe svm)
				movieGenres = list();
        			k = 1;
	      			for(x in seq(6,dim(movie)[2])){
                			if(movie[1,x] == 1){
		  				movieGenres[[k]] = genres[x-5,1];
		  				k = k+1;
                			}
        			}
				#On recupere la date du film (classe svm)
				date = as.numeric(strsplit(as.character(movie[1,3]),"-")[[1]][3]);
				#On ajoute aux trainData la note,le genre et la date
				for(x in seq(length(movieGenres))){
					#On extrait le nom du genre -syntaxe made in R...-	
					g = levels(movieGenres[[x]])[(levels(movieGenres[[x]]) == movieGenres[[x]])];
					#On s'en sert pour obtenir l'id du genre
					id = which(genres[,1] == g);
					#On n'ajoute qu'une fois la date [cf 1.]
					if(x == 1){
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

	#On transforme les data en data.frame (plus lisible et utilisable)
	testData = data.frame(matrix(unlist(testData), ncol = 3, byrow = TRUE));
	trainData = data.frame(matrix(unlist(trainData), ncol = 3, byrow = TRUE));
	#On nome les colonnes (plus lisible et utilisable pour svm)
	colnames(testData) = c("Like", "Genre","Year");
	colnames(trainData) = c("Like","Genre","Year");
	#On effecte la classification svm
	model = svm(Like~.,data=trainData,type="C-classification");
	#On save et plot le graphe de classification
	name = paste(c("res/plot",userId,".png"),collapse="");
	png(name);
	plot(model,trainData);
	dev.off();
	#On effectue la prediction a propos du modele svm 
	prediction = predict(model, testData[,-1], na.action = na.exclude);
	#On prepare la variable de prediction et on la retourne
	result = as.data.frame(prediction);
	result[,2] = testData[,2];
	result[,3] = testData[,3];
	return(result);
}
