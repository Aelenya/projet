# Fichier contenant des fonctions de base diverses

#Definition de l'operateur ternaire ?:
`?` <- function(x, y) {
  xs <- as.list(substitute(x))
  if (xs[[1]] == as.name("<-")) x <- eval(xs[[3]])
  r <- eval(sapply(strsplit(deparse(substitute(y)), ":"), function(e) parse(text = e))[[2 - as.logical(x)]])
  if (xs[[1]] == as.name("<-")) {
    xs[[3]] <- r
        eval.parent(as.call(xs))
  } else {
    r
  }
}

#Definition de l'ajout d'un element par 'tri a bulle' dans une (petite !) matrice
bubbleAdd = function(v, newElement){

	globalV = v;
	globalV = rbind(globalV, newElement);
	for(i in dim(globalV)[1]:2){
		if(globalV[i][1] < globalV[i-1][1]){
			tmp = globalV[i-1,];
			globalV[i-1,] = globalV[i,];
			globalV[i,] = tmp;
		}
	}
	return(globalV[-dim(globalV)[1],]);
}
