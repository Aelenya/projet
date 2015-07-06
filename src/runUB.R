library(Rmpi);
source("baseline.R");
source("import.R");

nProc = 4;
myRank = mpi.comm.rank(comm = 0);

users = getUsers();
tag = 1;

if(myRank == 0){
	movies = getMovies();
	nbUsers = dim(users)[1];
	nbMovies = dim(movies)[1];
	running = c(1,2,3);

	Y = matrix(ncol = nbMovies, nrow = nbUsers); #matrice Y generee par la baseline
	#On envoie les 3 premiers IDs
	mpi.isend(1, 2, 1, tag, comm = 0, request = 0);
	mpi.isend(2, 2, 2, tag, comm = 0, request = 0);
	mpi.isend(3, 2, 3, tag, comm = 0, request = 0);

	for(i in 1:5){
		#On attend un retour
		id = 0;
		for(p in seq(nProc-1)){
			mpi.irecv(id, 2, p, tag, comm = 0, request = p-1);
		}
		mpi.waitany(nProc-1, status = 0);
		
		#On receptionne le vecteur qu'on ajoute dans Y
		yPrime = vector(mode = "integer", length = nbMovies);
		yPrime = mpi.recv(yPrime, 1, id, tag, comm = 0, status = 0);
		Y[running[id],] = yPrime;
		
		if(i <= 2){
		mpi.send(i+3, 2, id, tag, comm = 0);
		running[id] = i+3;
		}

		if(id != 1) mpi.cancel(0);
		if(id != 2) mpi.cancel(1);
		if(id != 3) mpi.cancel(2); 
	}
	id = mpi.recv(id, 2, id, tag, comm = 0, status = 0);
	yPrime = vector(mode = "integer", length = nbMovies);
	yPrime = mpi.recv(yPrime, 1, id, tag, comm = 0, status = 0);
	Y[running[id],] = yPrime;
	cancel = -1;
	for(a in seq(nProc-1)){
		mpi.send(cancel, 2, a, tag, comm = 0);
	}
}

if(myRank != 0){
	while(1){
		userId = 0;
		userId = mpi.recv(userId, 2, 0, tag, comm = 0, status = 0);
		if(userId == -1) break;
		user = users[userId,];
		res = UB(user);
		mpi.send(myRank, 2, 0, tag, comm = 0);
		mpi.send(res, 1, 0, tag, comm = 0);
	}
}
cat("\n ",myRank," a fini son boulot !");
mpi.quit();
