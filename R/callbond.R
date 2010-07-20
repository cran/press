callBond<-function(){
#require("gWidgets")
options("guiToolkit"="RGtk2")

##read data files
#B<-read.csv("BioMath/pdb/virtBond.csv",header=T)
B<-virtBond

##function that draws the plots
updatePlot <- function(h,...){

if(svalue(denseradio)=="Frequency")
densevalue<-TRUE
if(svalue(denseradio)=="Density")
densevalue<-FALSE
#counter<-0
#counter<-counter+1
#if(counter>1)
#dev.set(currentdev)

mysplot<-function(a,b){
mydata<-B
laball<-c(a,b)
      laball<-as.data.frame(laball,ncol=1)
      rownames(laball)<-c('res1','res2')
      laball<-na.omit(subset(laball,laball!='Any'))
      if (nrow(laball)>=1){    ## Specify more than (>=)1 residues use scatter3d best
      for (i in rownames(laball)){
           mydata<-mydata[which(as.character(mydata[,i])==rep(laball[i,1],nrow(mydata))),]
          }
     }
if(dim(mydata)<1 && svalue(selectres1) != "" && svalue(selectres2) != "" )
gmessage("No Data")
else{
low<-round(min(rm.outlier(mydata$Bond))-0.05,1)
high<-round(max(rm.outlier(mydata$Bond))+0.05,1)
hist((mydata$Bond),breaks=svalue(bondbinslider),freq=densevalue,col="dark blue",
 border="lightslategray",main="",xlim=c(low,high),
xlab=paste("virtual bond length between",a,b))
} }

a<-svalue(selectres1)
b<-svalue(selectres2)
if(a!="Any" | b!="Any")
mysplot(a,b)
else
##aggregate plot of bond lengths
hist(B[,3],breaks=600,freq=FALSE,main="Histogram of Bond Length",
border="lightslategray")
#currentdev<- dev.cur()
}

##GUI layout
selectres1<- gcombobox(c("","Any","ALA", "ARG", "ASN", "ASP","CYS", "GLN", "GLU", "GLY", "HIS", "ILE", "LEU", "LYS", "MET", "PHE", "PRO", "SER", "THR", "TRP", "TYR", "VAL")
, handler=updatePlot)
selectres2<- gcombobox(c("","Any","ALA", "ARG", "ASN", "ASP","CYS", "GLN", "GLU", "GLY", "HIS", "ILE", "LEU", "LYS", "MET", "PHE", "PRO", "SER", "THR", "TRP", "TYR", "VAL")
, handler=updatePlot)
bondbinslider<-gslider(from=1,to=500,by=5,value=250, handler=updatePlot)

window <- gwindow("Virtual Bond Distance")
size(window)<-c(800,500)
BigGroup <- ggroup(horizontal=FALSE,cont=window)
BiggerGroup<-ggroup(cont=window)
group <- ggroup(horizontal=FALSE, container=BigGroup)
tmp <- gframe("Residue 1", container=group)
add(tmp, selectres1)
tmp <- gframe("Residue 2", container=group)
add(tmp, selectres2)
#tmp <- gframe("Slider for Bin Size", container=group)
#add(tmp, bondbinslider)
binlabel<-glabel("Slider for Number of Bins", container=group)
bondbinslider<-gslider(from=1,to=500,by=5,value=250,container=group , handler=updatePlot)
denseradio<-gradio(c("Frequency","Density"), container=group, handler=updatePlot)




plotgroup<-ggroup(horizontal=FALSE, container=BigGroup)

graphicsbond<- ggraphics()
add(BiggerGroup, graphicsbond)
}
