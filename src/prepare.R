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
				y = userRatings[[j]][1,2];

				#On recupere les genres du film (label svm)
				movieGenres = list();
        			k = 1;
	      			for(x in seq(6,dim(movie)[2])){
                			if(movie[1,x] == 1){
		  				movieGenres[[k]] = genres[x-5,1];
		  				k = k+1;
                			}
        			}
				#On recupere la date du film (label svm)
				date = as.numeric(strsplit(as.character(movie[1,3]),"-")[[1]][3]);
				#On ajoute aux trainData la note,le genre et la date
				#On ne conserve que 1 seul genre (cf rapport)
				select = sample(1:length(movieGenres),1);
				for(x in seq(length(movieGenres))){
					#On extrait le nom du genre -syntaxe made in R...-	
					g = levels(movieGenres[[x]])[(levels(movieGenres[[x]]) == movieGenres[[x]])];
					#On s'en sert pour obtenir l'id du genre
					id = which(genres[,1] == g);
					if(x == select){
						trainData[[A]] = c(y,id,date);
						A = A + 1;
						break;
					}
				}
				#On peut break pour ce film et passer au suivant
				break;
			} 
		}
	}
	#On transforme les data en data.frame (plus lisible et utilisable)
	testData = data.frame(matrix(unlist(testData), ncol = 3, byrow = TRUE));
	trainData = data.frame(matrix(unlist(trainData), ncol = 3, byrow = TRUE));
	#On nomme les colonnes (plus lisible et utilisable pour svm)
	colnames(testData) = c("Like", "Genre","Year");
	colnames(trainData) = c("Like","Genre","Year");
	#On effecte la classification svm
	model = svm(Like~.,data=trainData,type="C-classification",kernel="polynomial");
	tryCatch({
		#On save et plot le graphe de classification
		name = paste(c("res/plot",userId,".png"),collapse="");
		png(name);
		plot(model,trainData);
		dev.off();
	},
	error = function(e){
		cat("/!\\ Variable constante pour l'utilisateur ",userId," /!\\ \n");
	});
	tryCatch({
		#On effectue la prediction a partir du modele svm 
		prediction = predict(model, testData[,-1], na.action = na.exclude);
		#On prepare la variable de prediction et on la retourne
		result = as.data.frame(prediction);
		result[,2] = testData[,2];
		result[,3] = testData[,3];
		#On insere l'id des films pour chaque element
		clean = result[-which(is.na(result[,3])),];
		clean[,4] = testId;
		colnames(clean) = c("Like","Genre","Year","Movie.id");
		#On ordonne par ordre decroissant de note et d'id
		clean = clean[order(clean$Like, decreasing = TRUE), ];
		return(clean);
	},
	error = function(e){
		cat("/!\\ Aucune prediction pour l'utilisateur ", userId, " /!\\ \n");
		return(-1);
	});
}

#Fonction dans laquelle on base svm sur des features provenant de IMDB
itemCold.IMDB = function(userId, testId, class1, class2){

	if(class1 == class2) stop("Il faut selectionner 2 classes differentes");

	info1 = -1; info2 = -1;
	if(class1 == "composers" || class2 == "composers"){
		info1 = getComposers();
	}
	if(class1 == "directors" || class2 == "directors"){
		if(info1 == -1){
			info1 = getDirectors();
		}
		else info2 = getDirectors();
	}
	if(class1 == "keywords" || class2 == "keywords"){
		if(info1 == -1){
			info1 = getKeywords();
		}
		else info2 = getKeywords();
	}
	if(class1 == "writers" || class2 == "writers"){
		if(info1 == -1){
			info1 = getWriters();
		}
		else info2 = getWriters();
	}
	if(class1 == "countries" || class2 == "countries"){
		if(info1 == -1){
			info1 = getCountries();
		}
		else info2 = getWriters();
	}
	if(info1 == -1 || info2 == -1) stop("Aucune classe valide selectionee");

	userRatings = list();
	k = 1;
	for(i in seq(dim(ratings)[1])){
		if(ratings[i,1] == userId){
			userRatings[[k]] = ratings[i,c(2,3)];
			k = k + 1;
		}
	}
	movies = getMovies();
	testMovies = movies[testId,];
	trainMovies = movies[-testId,];

	testData = list();
	A = 1;
	for(i in seq(dim(testMovies)[1])){
		movie = movies[testMovies[i,1],];
		col1 = list(); col2 = list();
		k1 = 1; k2 = 1;
		#Pour chaque element dans la colonne importee...
		for(x in seq(dim(info1)[1])){
			#Si a la position de l'id du film on a autre chose que 0...
			if(info1[x,2+movie[1]] > 0){
				#On ajoute l'id de l'import (pas besoin des char precis)
				col1[[k1]] = info1[x,1];
				k1 = k1 + 1;
			}
		}
		#Meme chose pour la deuxieme colonne...
		for(x in seq(dim(info2)[1])){
			if(info2[x,2+movie[1]] > 0){
				col2[[k2]] = info2[x,1];
				k2 = k2+1;
			}
		}
	 #########################
		for(x in seq(1,length(col1))){
			if(x == 1){
				testData[[A]] = c(0, as.integer(col1[[x]]), as.integer(col2[[x]]));
			}
		}
	}
	traindata = list();
	A = 1;
	for(i in seq(dim(trainMovies)[1])){
		for(j in seq(length(userRatings))){
			if(trainMovies[i,1] == userRatings[[j]][1,1]){
				movie = movies[trainMovies[i,1],];
				y = userRatings[[j]][1,2];
				for(x in seq(dim(info1)[1])){
					if(info1[x,2+movie[1]] > 0){
						col1[[k1]] = info1[x,1];
						###############
					}
				}
			}
		}
	}
}
