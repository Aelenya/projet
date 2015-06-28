library(Rmpi);

source("prepare.R");

nProc = 4;
myRank = mpi.comm.rank(comm = 0);
nbData = length(init());
tag = 0;
testId = vector(mode="integer", length=nbData);

if(myRank == 0){
	u = getUsers();
	user = 1;
	testId = init();
	first = 1;
}

#On envoie les indices de testId
for(a in seq(nbData)){
	testId[a] = mpi.bcast(testId[a], 1, rank = 0, comm = 0);
}

if(myRank == 0){
	#while(user-1 != dim(u)[1]){
	while(user-1 != 10){
		if(first == 1){
			for(id in seq(nProc-1)){
				value = u[user,1];
				mpi.send(value, 2, id, tag, comm = 0);
				user = user+1;
			}
			first = -1;
			
		}
		else{
			id = 0;
			#id = mpi.recv(id, 2, 1, tag, comm = 0, status = 0);
			for(p in seq(nProc-1)){
				mpi.irecv(id, 2, p, tag, comm = 0, request = p-1);
			}
			mpi.waitany(nProc-1,status = 0);
	
			mpi.send(u[user,1], 2, id, tag, comm = 0);
			user = user+1;
			if(id != 1) mpi.cancel(0);
			if(id != 2) mpi.cancel(1);
			if(id != 3) mpi.cancel(2);
		}
	}
	id = mpi.recv(id, 2, id, tag, comm = 0, status = 0);
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
		res = itemCold.BASIC(userId, testId);
		#ecrit dans un fichier le resultat obtenus
		for(k in seq(dim(res)[1])){
			name = paste(c("res/data%",userId),collapse="");
			write(as.matrix(res[k,]),file=name,ncolumns=3,sep="\t",append=TRUE);
		}
		mpi.send(myRank, 2, 0, tag, comm = 0);
	}
}



mpi.quit();

