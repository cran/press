
distangNoPlots<-function(a,b,c,x0){
ttang<-Angle_Bond
## function to get the distribution of dihedral angle a,b,c.and the probability of x0
angle.test<-c(seq(range(ttang$BondAngles)[1],range(ttang$BondAngles)[2],by=0.1),range(ttang$BondAngles)[2])
ang<-ttang[(ttang$res1==a & ttang$res2==b & ttang$res3==c)|
 (ttang$res1==c & ttang$res2==b & ttang$res3==a),]
pmf<-c()
n<-length(ang$BondAngles)
if (n>2){
####for gaussian multiplier is 1.059 in smoothing methods
#binwidth<-2.15*sd(ang$BondAngles)*(n^(-.2))
bandwidth<-1.059*sd(ang$BondAngles)*(n^(-.2))
#brk1<-round((range(ang$BondAngles)[2]-range(ang$BondAngles)[1])/binwidth)
brk2<-round((range(ang$BondAngles)[2]-range(ang$BondAngles)[1])/bandwidth)
dd<-density(ang$BondAngles,bw =bandwidth,kernel="gaussian")
lev <- approx(dd$x,dd$y,angle.test)$y
lev[!is.na(lev)]<-lev[!is.na(lev)]/sum(lev[!is.na(lev)])
lev[is.na(lev)]<-10^(-5)
opt<-min(-log(lev)) 
pmf<--log(approx(angle.test,lev,x0)$y)
}else {
pmf<-NA
opt<-NA
}
return(c(pmf,opt))
}
#da<-distangNoPlots("GLY","ASN","GLY",40.3)
##only for test
#da<-distangNoPlots("GLY","ASN","PRO",100.3)

