
#Script R construisant les donnees pour LambdaMART
source("prepare.R");
source("import.R");
source("checkData.R");

testId = init();
movies = getMovies();
users = getUsers();
ratings = getRatings();

#Valeurs d'apprentissage
genres = getGenres();
maxGenres = dim(genres)[1];
directors = getDirectors();
maxDirectors = trunc(labelAverage.IMDB(directors))+1;
writers = getWriters();
maxWriters = trunc(labelAverage.IMDB(writers))+1;
countries = getCountries();
maxCountries = trunc(labelAverage.IMDB(countries))+1;
composers = getComposers();
maxComposers = trunc(labelAverage.IMDB(composers))+1;

testMovies = movies[testId,];
trainMovies = movies[-testId,];

userId = 1;
userRatings = list();
k = 1;
for(i in seq(dim(ratings)[1])){
	if(ratings[i,1] == userId){
        	userRatings[[k]] = ratings[i,c(2,3)];
                k = k + 1;
        }

}

count = 1;
tic = 0;
sink("trainMART");
#Pour chaque film d'entrainement...
for(i in seq(dim(trainMovies)[1])){

	#S'il a ete note par l'utilisateur...
	for(j in seq(length(userRatings))){
		#On prepare une nouvelle ligne pour le fichier train
		line = matrix(); 
		y = 0; cou = list(); com = list(); wri = list(); movieGenres = list();
		if(trainMovies[i,1] == userRatings[[j]][1,1]){
			#On recupere les infos sur le film
			movie = movies[trainMovies[i,1],];
			y = userRatings[[j]][1,1];
			#On recupere les id des genres (tous, contrairement a svm !)
                        k = 1;
                        for(x in seq(6,dim(movie)[2])){
                        	if(movie[1,x] == 1){
                                	movieGenres[[k]] = genres[x-5,2];
                                        k = k+1;
                                }
                        }
			#On recupere les pays des films
			tmp = which(countries[,2 + as.integer(movie[1])] > 0);
			for(x in seq(length(tmp))){
				cou[[x]] = countries[tmp[x],1];
			}
			#On recupere les compositeurs des films
			tmp = which(composers[,2 + as.integer(movie[1])] > 0);
			for(x in seq(length(tmp))){
				com[[x]] = composers[tmp[x],1];
			}
			#On recupere les scenaristes des films
			tmp = which(writers[,2 + as.integer(movie[1])] > 0);
			for(x in seq(length(tmp))){
				wri[[x]] = writers[tmp[x],1];
			}
		#On construit la ligne selon le format jforests suivant :
#note,genre1,...,genreN,country1,...,countryN,composer1,...,composerN,writer1,...,writerN
			line[1] = y;
			line[2:2+maxGenres] = 0;
			line[2+as.integer(movieGenres)] = 1;
			x = 2 + maxGenres + 1;
			line[x:x+maxCountries] = 0;
			for(k in seq(length(cou))){
				line[k+x] = cou[k];
				if(k > x+maxCountries) break;
			}
			cat(y," ",sep="");
			for(k in seq(length(line))){
				cat(k,":",line[[k]]," ",sep="");
			}
			cat("\n");
			sink();
			count = count + 1;
			tic = 1;res = line;
			break;
		}
		if(tic == 1) break;
	}
	if(tic == 1) break;
}
