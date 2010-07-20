
## function for calculating torsional angles between 4 connected residues of a specific pdb file
tang<-function(p1,p2,p3,p4){
 d1 <- p2-p1
 d2 <- p3-p2
 d3 <- p4-p3
 ## to generate 2 normal vectors of vectors d1&d2 and d2&d3
 u1<-(d1[c(2,3,1)]*d2[c(3,1,2)])-(d2[c(2,3,1)]*d1[c(3,1,2)])
 u2<-(d2[c(2,3,1)]*d3[c(3,1,2)])-(d3[c(2,3,1)]*d2[c(3,1,2)])
 ctor <-sum(u1 * u2)/sqrt(sum(u1 * u1) * sum(u2 * u2))
 ctor[ctor>1] <- 1
 ctor[ctor< -1] <- -1
 ##to calculate the angl between the two normal vectors
 torp<-acos(ctor)*(180/pi)
 if (sum(u1*((u2[c(2,3,1)]*d2[c(3,1,2)])-(u2[c(3,1,2)]*d2[c(2,3,1)]))) < 0)
 torp <- -torp
 return(torp)
}

## function for compuations of all tortional angles of a protein structure with protein name: id and pdb file
torsion<-function(id,pdb){
Tor<-NULL
calpha<-pdb$atom[pdb$calpha, c("resid","x","y","z","resno")]
calpha<-data.frame(calpha)
nP<-nrow(calpha)
if (nP<4 )  return(NULL) 
resid=paste(calpha$resid)
x=as.numeric(paste(calpha$x))
y=as.numeric(paste(calpha$y))
z=as.numeric(paste(calpha$z))
resno=as.numeric(paste(calpha$resno))
calpha = data.frame(resid,x,y,z,resno)
p.calpha<-data.matrix(calpha[,2:4])


for (j in 1:(nP-3)){
Tor<-c(Tor,tang(as.numeric(p.calpha[j,]),as.numeric(p.calpha[j+1,]),
as.numeric(p.calpha[j+2,]),as.numeric(p.calpha[j+3,])))
}
ID<-rep(as.character(id),(nrow(p.calpha)-3))
d<-matrix(cbind(
as.numeric(calpha[,2][1:(nP-3)]),as.numeric(calpha[,2][4:(nP)])
,as.numeric(calpha[,3][1:(nP-3)]),as.numeric(calpha[,3][4:(nP)])
,as.numeric(calpha[,4][1:(nP-3)]),as.numeric(calpha[,4][4:(nP)])),ncol=6)
distance<-sqrt((d[,1]-d[,2])^2+(d[,3]-d[,4])^2+(d[,5]-d[,6])^2)
Torsion<-data.frame(calpha[,5][1:(nP-3)],calpha[,5][2:(nP-2)],calpha[,5][3:(nP-1)],calpha[,5][4:(nP)]
,calpha[,1][1:(nP-3)],calpha[,1][2:(nP-2)],calpha[,1][3:(nP-1)],calpha[,1][4:(nP)]
,distance,Tor)
colnames(Torsion)<-c("resNo1","resNo2","resNo3","resNo4","res1","res2","res3","res4",
"distance","Torsion")
dle.id=(Torsion$resNo4==Torsion$resNo1+3)& (Torsion$resNo3==Torsion$resNo1+2) & (Torsion$resNo3==Torsion$resNo2+1)
Torsion=Torsion[dle.id,]

#write.table(Torsion,paste("p:/BioMath/pdb/Torsion/",as.character(id),".txt",sep=""),row.names=FALSE,sep="\t")
return(Torsion)
}


