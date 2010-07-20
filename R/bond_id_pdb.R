## function for calculating bond length between two neighbouring residues of a specific pdb file
bond<-function(id,pdb){
calpha<-pdb$atom[pdb$calpha, c("resid","x","y","z","resno")]               
calpha = data.frame(calpha)
nP=nrow(calpha)
if (nP<1 )  return(NULL) 
resid=paste(calpha$resid)
x=as.numeric(paste(calpha$x))
y=as.numeric(paste(calpha$y))
z=as.numeric(paste(calpha$z))
resno=as.numeric(paste(calpha$resno))
calpha = data.frame(resid,x,y,z,resno)
p.calpha<-data.matrix(calpha[,2:4])

distance<-matrix(p.calpha[1:(nP-1),]-p.calpha[2:nP,],ncol=3)
d<-sqrt((distance^2)[,1]+(distance^2)[,2]+(distance^2)[,3]) 
ID<-rep(as.character(id),nP-1)
Bond<-data.frame(resno[1:(nP-1)],resno[2:nP],
resid[1:(nP-1)],resid[2:nP],d)
colnames(Bond)<-c("resNo1","resNo2","res1","res2","Bond")
dle.id<-(Bond$resNo2==Bond$resNo1+1)
Bond<-Bond[dle.id,]
return(Bond)
}

