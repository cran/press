
data.ata<-function(id,pdb){
Angle<-angle(id,pdb)
Torsion<-torsion(id,pdb)

##combind the Angle, Bond and Torsion files to a-t-b, and t1-b-t2 data set

torsion.temp<-Torsion[which(Torsion$resNo1==(Torsion$resNo2-1) & 
Torsion$resNo2==(Torsion$resNo3-1) &
Torsion$resNo3==(Torsion$resNo4-1) ),]
if (nrow(torsion.temp)<nrow(Torsion)){
Torsion<-torsion.temp
}
angle.temp<-Angle[which(Angle$resNo1==(Angle$resNo2-1) & 
Angle$resNo2==(Angle$resNo3-1) ),]
if (nrow(angle.temp)<nrow(Angle)){
Angle<-angle.temp
}
aa<-cbind(Angle[1:nrow(Angle)-1,c(1:3,8)],Angle[2:nrow(Angle),c(1:3,8)])
colnames(aa)<-c("resNo1","resNo21","resNo31","angle1","resNo22","resNo32","resNo4","angle2")
aa<-aa[which(aa$resNo1==(aa$resNo4-3)),]
ata<-data.frame(Torsion$resNo1,Torsion$resNo2,Torsion$resNo3,Torsion$resNo4,
Torsion$res1,Torsion$res2,Torsion$res3,Torsion$res4,
aa$angle1,Torsion$Torsion,aa$angle2,rep(id,nrow(Torsion)))
colnames(ata)<-c("resNo1","resNo2","resNo3","resNo4","res1","res2","res3","res4",
"angle1","torsional","angle2","protein")
ata$torsional[ata$torsional<0]<-ata$torsional[ata$torsional<0]+360
return(ata)
}




#library(bio3d)


#id<-"F1_1009"
#pdb<-read.pdb("P:/BioMath/decoys/decoylist/F1_1009.pdb")
#ata<-data.ata(id,pdb)
#tat<-data.tat(id,pdb)