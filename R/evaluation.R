pressangle<-function(id,pdb){
pa<-c()
Angle<-angle(id,pdb)
for (i in 1:nrow(Angle)){
na<-distangNoPlots(as.character(Angle[i,4]),as.character(Angle[i,5]),
as.character(Angle[i,6]),(Angle[i,8]))
pa<-rbind(pa,na)
}
op<-par(mfrow=c(2,1))
plot(c(1:nrow(Angle)),pa[,1],type="n",xlab="Angle Index",ylab="Potential Estimation(-ln(Density))")
title(main=paste("Potential Mean Estimation for \neach Residue-level Virtual Angle in",id),
cex.main = 1.2,   font.main= 4)
lines(c(1:nrow(Angle)),pa[,1],col=3,type="s",lty=1)
lines(c(1:nrow(Angle)),pa[,2],col=6,type="s",lty=2)
legend("topright", floor(max(pa[pa!='Inf'])), c("Estimated", "Optimal"), col = c(3,6),
      text.col = c(3,6), lty = c(1,  2), pch = c( 3, 4),  cex=.8,
      merge = TRUE, bg = 'gray90')
points(which.max(pa[,1]),max(pa[,1]),pch="*",col="dark red",cex=2)
text(which.max(pa[,1]),max(pa[,1]),paste("\nIndex",which.max(pa[,1]),"-->"),pos=2,col="dark red",cex=.8)
#text(which.max(pa[,1]),max(pa[,1]),paste("\nwith Angle=",
#round(as.numeric(Angle[which.max(pa[,1]),8]))),pos=4,col="dark red",cex=.8)
#text(which(pa[,1]==Inf),11,"Inf",pos=3,col="dark red",cex=0.8)

plot(c(1:nrow(Angle)),Angle$BondAngles,type="n",xlab="Angle Index",ylab="virtual Bond Angle ")
title(main=paste("Residue-level Virtual Bond Angles in",id),
cex.main = 1.2,   font.main= 4)
lines(c(1:nrow(Angle)),Angle$BondAngles,col=3,type="s",lty=1)
par(op)
}

pressbond<-function(id,pdb){
pb<-c()
Bond<-bond(id,pdb)
for (j in 1:nrow(Bond)){
nl<-distbondNoPlots(as.character(Bond[j,3]),as.character(Bond[j,4]),(Bond[j,5]))
pb<-rbind(pb,nl) 
}
op<-par(mfrow=c(2,1))
plot(c(1:nrow(Bond)),pb[,1],type="n",xlab="Bond Index",ylab="Potential Estimation(-ln(Density))")
title(main=paste("Potential Mean Estimation for \neach Residue-level Virtual Bond Length in",id),
cex.main = 1.2,   font.main= 4)
lines(c(1:nrow(Bond)),pb[,1],col=3,type="s",lty=1)
lines(c(1:nrow(Bond)),pb[,2],col=6,type="s",lty=2)
legend("topright", floor(max(pb[pb!='Inf'])), c("Estimated", "Optimal"), col = c(3,6),
       text.col = c(3,6), lty = c(1,  2), pch = c( 3, 4),  cex=.8,
       merge = TRUE, bg = 'gray90')
points(which.max(pb[,1]),max(pb[,1]),pch="*",col="dark red",cex=2)
text(which.max(pb[,1]),max(pb[,1]),paste("\nIndex",which.max(pb),"-->"),pos=2,col="dark red",cex=.8)
#text(which.max(pb[,1]),max(pb[,1]),paste("\nwith Length=",
#round(as.numeric(Bond[which.max(pb[,1]),5]),2)),pos=4,col="dark red",cex=.8)
#text(-1,5,expression(bar(E)),pos=4,col="dark blue")
#text(0.5,5,paste("=",round(mean(pb[,1]),2)),pos=4,col="dark blue")

plot(c(1:nrow(Bond)),Bond$Bond,type="n",xlab="Bond Index",ylab="residue distance (A)")
title(main=paste("Residue-level Virtual Bond Length in",id),
cex.main = 1.2,   font.main= 4)
lines(c(1:nrow(Bond)),Bond$Bond,col=3,type="s",lty=1)
}


presscont1<-function(id,pdb){
dst1<-dst1

ata<-data.ata(id,pdb)
est1<-c()
for (j in 1:nrow(ata)){
est1[j]<-dst1$z[which.min(abs(dst1$x-as.numeric(ata[j,9]))),
which.min(abs(dst1$y-as.numeric(ata[j,10])))]
}
perc1.allowed<-100*length(est1[which(est1>0.00001804)])/length(est1)   #90% level
perc1.favoured<-100*length(est1[which(est1>0.00004661)])/length(est1)  #75% level
perc1.mfav<-100*length(est1[which(est1>0.00009893)])/length(est1)      #50% level
perc1<-round(c(perc1.allowed,perc1.favoured,perc1.mfav),2)
decoy.ramachandran.plot1(as.numeric(ata[,9]), as.numeric(ata[,10]),perc1,
             x.grid=dst1$x, y.grid=dst1$y, z.grid=dst1$z,
             plot.title=paste("scatter plots over general contours for protein",id),
            levels=c(0,0.00001804,0.00004661,0.00009893, 0.001823939),
             col=c('#FFFFFF','#B3E8FF','#7FD9FF','dodgerblue3'))

}


presscont2<-function(id,pdb){

dst2<-dst2
ata<-data.ata(id,pdb)
est2<-c()
for (j in 1:nrow(ata)){
est2[j]<-dst2$z[which.min(abs(dst2$x-as.numeric(ata[j,11]))),
which.min(abs(dst2$y-as.numeric(ata[j,10])))]
}
perc2.allowed<-100*length(est2[which(est2>0.00001678)])/length(est2)   #90% level
perc2.favoured<-100*length(est2[which(est2>0.00003428)])/length(est2)  #75% level
perc2.mfav<-100*length(est2[which(est2>0.00008791)])/length(est2)      #50% level
perc2<-round(c(perc2.allowed,perc2.favoured,perc2.mfav),2)

decoy.ramachandran.plot2(as.numeric(ata[,11]), as.numeric(ata[,10]),perc2,
             x.grid=dst2$x, y.grid=dst2$y, z.grid=dst2$z,
             plot.title=paste("scatter plots over general contours for protein",id),
            levels=c(0,0.00001678,0.00003428, 0.00008791,0.001822614),
             col=c('#FFFFFF','#C0FFC0','#80FF80','chartreuse3'))

}
