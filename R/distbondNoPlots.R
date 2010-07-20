distbondNoPlots<-function(a,b,x0){

ttbond<-virtBond
ttbond<-ttbond[-which.max(ttbond$Bond),]
bond.test<-c(seq(range(ttbond$Bond)[1],range(ttbond$Bond)[2],by=0.001),range(ttbond$Bond)[2])
## function to get the distribution of virtual bond length, and the probability of x0
bond<-ttbond[(ttbond$res1==a & ttbond$res2==b)  ,]
bond<-bond$Bond
n=length(bond)
#for gaussian multiplier is 1.059 in smoothing methods
## for Epanechnikov multiplier is 2.214*1.059
#binwidth<-2.15*sd(bond)*(n^(-.2))
bandwidth<-1.059*sd(bond)*(n^(-.2))
rang<-max(bond)-min(bond)
#brk1<-round(rang/binwidth) # breaks for histogram with optimal bin width
brk2<-round(rang/bandwidth)
dd<-density(bond ,bw =bandwidth,kernel="gaussian")
lev <- approx(dd$x,dd$y,bond.test)$y
lev[!is.na(lev)]<-lev[!is.na(lev)]/sum(lev[!is.na(lev)])
lev[is.na(lev)]<-10^(-5)
opt<-min(-log(lev)) 
pmf<--log(approx(bond.test,lev,x0)$y)
return(c(pmf,opt))
}


