##function for calculating bond angles between 3 connected residues of a specific pdb file
bang<-function(p1,p2,p3){
d1 <- p1-p2
d2 <- p3-p2
t<-acos(d1%*%d2/sqrt(sum(d1^2))/sqrt(sum(d2^2)))*180/pi
return(t)
}

## function for calculating bond angle and 1-3 distance for any pdb file
angle<-function(id,pdb){

calpha<-pdb$atom[pdb$calpha, c("resid","x","y","z","resno")]
calpha = data.frame(calpha)
nP=nrow(calpha)
if (nP<3 )  return(NULL) 
resid=paste(calpha$resid)
x=as.numeric(paste(calpha$x))
y=as.numeric(paste(calpha$y))
z=as.numeric(paste(calpha$z))
resno=as.numeric(paste(calpha$resno))
calpha = data.frame(resid,x,y,z,resno)
p.calpha<-data.matrix(calpha[,2:4])

Ang<-NULL
for (j in 1:(nP-2))  
   Ang<-c(Ang,bang(as.numeric(p.calpha[j,]),as.numeric(p.calpha[j+1,]),as.numeric(p.calpha[j+2,])))

ID<-rep(as.character(id),nP-2)

d=cbind(foo=head(p.calpha,nP-2),p.calpha[-(1:2),])[,c(1,3,5,2,4,6)]
distance<-sqrt((d[,1]-d[,2])^2+(d[,3]-d[,4])^2+(d[,5]-d[,6])^2)


Angle<-data.frame(resno[1:(nP-2)],resno[2:(nP-1)],resno[3:(nP)],
     resid[1:(nP-2)],resid[2:(nP-1)],resid[3:(nP)],distance,Ang)
colnames(Angle)<-c("resNo1","resNo2","resNo3","res1","res2","res3","distance","BondAngles")

#print(dim(Angle))

dle.id=(Angle$resNo3==Angle$resNo1+2) & (Angle$resNo3==Angle$resNo2+1)

#print((Angle)[!dle.id,])
Angle=Angle[dle.id,]
return(Angle)
}
