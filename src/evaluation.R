# Fichier contenant les fonctions servant a l'evaluation de la qualite de notre recommandation

DCG = function(y,yPrime,k){
cat("\n  #############################");
	somme = 0;
	rank = 1;
	for(i in seq(dim(yPrime)[1])){
		id = yPrime[i,4];
		rel = -1;
		if(dim(y[which(y[,4] == id),])[1] > 0){
			rel = as.integer(yPrime[i,1]);
		}
		if(rel != -1){
			num = 2^rel - 1;
			den = log2(i + 1);
			somme = somme + (num/den);
			rank = rank + 1;
	#tester les valeurs
cat("\n id = ",id," rel = ",rel," rank = ",rank," pos = ",i);
		}
		if(rank == k) break;
	}
	return(somme);
}

#y = vraie (test data) valeur, yPrime = train data, k = lvl de troncation
NDCG = function(y,yPrime,k){
	#On supprime les eventuelles lignes 'fantomes' (notes pas dans ]0,5])
	y = y[which(y[,1] > 0),];
	res = DCG(y,yPrime,k)/DCG(y,y,k);
	return(res);
}

rel = function(train, test){
	trainId = train[1,4];
  	trainLike = as.integer(train[1,1]);
  	realLike = test[which(test[,4] == trainId),1];
	if(length(realLike) > 0 && realLike >= 0){
  	
	}
	else return(-1)

}

getTest = function(userId){

	r = getRatings();
	m = getMovies();
	testMovies = m[testId,];
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
                        if(x == 1){
                                testData[[A]] = c(-1,id,date);
                        }
                        else{
                                testData[[A]] = c(-1,id,NA);
                        }
                        A = A+1;
                }
        }

	testData = data.frame(matrix(unlist(testData), ncol = 3, byrow = TRUE));
	testData = testData[-which(is.na(testData[,3])),];
	testData[,4] = testId;
	
	for(i in seq(dim(r)[1])){
	  if(r[i,1] == userId){
	    movId = which(testId == r[i,2]);
	    if(length(movId) > 0){
	      tId = which(testData[,4] == movId);
	      testData[tId,1] = r[i,3];
	    }
	  }
	}
	colnames(testData) = c("Like", "Genre","Year","Movie.id");
	testData = testData[order(testData$Like,decreasing=TRUE),];
	#On retourne uniquement les films notes par l'utilisateur
	return(testData[-which(testData[,1] == -1),]);
}
