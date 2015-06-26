# Fichier contenant les fonctions servant a l'evaluation de la qualite de notre recommandation

DCG = function(r,y,k){

	sum = 0;
	for(i in seq(dim(y)[1])){
		I = (r[i] >= k) ? 1 : 0;
		if(I){
			num = 2^y[i]-1;
			den = log2(r[i]+1);
			sum = sum + (num/den);
		}
	}
	return(sum);

}

NDCG = function(y,yPrime,k){

	

}



evaluate = function(movies, predictions, k, method){

	if(method == "MP"){
		y = MP(movies);
		yPrime = predictions;
		return(NDCG(y,yPrime,k));
	}
	else if(method == "UB"){

	}
	else{
		print("Methode de baseline inconnue");
	}

}

