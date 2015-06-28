
library(Rmpi);

if( mpi.comm.rank(comm = 0) == 0){
	cat("Je suis numero 0 !");
}
if( mpi.comm.rank(comm = 0) != 0){
	cat(mpi.comm.rank(comm=0)," travailleur\n");
}


mpi.quit();
