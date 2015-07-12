# Fichier contenant toutes les fonctions d'import des donnees, ajouter celles qui sont
# encore necessaires (elles n'y sont pas toutes la)

pathMovies = "../data/movies/";
pathUsers = "../data/users/";


# Infos relatives aux films avec les ajouts de IMDb
getMovies = function(){

	movies = read.table(paste(pathMovies,"u.item",sep=""), header = TRUE, sep = "|");
	return(movies);

}

getComposers = function(){

	composers = read.table(paste(pathMovies,"u.item.composers",sep=""), header = FALSE, sep = "|");
	colnames(composers) = c("Composer.id","Name");
	return(composers);
}

getCountries = function(){

	countries = read.table(paste(pathMovies,"u.item.countries",sep=""), header = FALSE, sep = "|");
	colnames(countries) = c("Country.id","Name");
	return(countries);
}

getDirectors = function(){

	directors = read.table(paste(pathMovies,"u.item.directors",sep=""), header = FALSE, sep = "|");
	colnames(directors) = c("Director.id","Name");
	return(directors);
}

getKeywords = function(){

	keywords = read.table(paste(pathMovies,"u.item.keywords",sep=""), header = FALSE, sep = "|");
	colnames(keywords) = c("Keyword.id","Name");
	return(keywords);
}

getWriters = function(){

	writers = read.table(paste(pathMovies,"u.item.writers",sep=""), header = FALSE, sep = "|");
	colnames(writers) = c("Writer.id","Name");
	return(writers);
}



# Infos relatives aux utilisateurs
getUsers = function(){

	users = read.table(paste(pathUsers,"u.user",sep=""), header = TRUE, sep = "|");
	return(users);
}

getRatings = function(){

	ratings = read.table(paste(pathUsers,"u.data",sep=""), header = TRUE, sep = "\t");
	return(ratings);

}

# Retourne le nom des genres possibles et leur id
getGenres = function(){

	genres = read.table(paste(pathUsers,"u.genre",sep=""), header = TRUE, sep = "|");
	return(genres);

}

getJobs = function(){

	jobs = read.table(paste(pathUsers,"u.occupation",sep=""), header = TRUE, sep = "|");
	return(jobs);

}
