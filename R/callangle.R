callAngle<-function(){
#require("gWidgets")
options("guiToolkit"="RGtk2")

##read data files
#A<-read.csv("BioMath/pdb/Angle_Bond.csv",header=T)
A<-Angle_Bond

##function that draws the plots
updatePlot <- function(h,...){
#counter<-0
#counter<-counter+1
#if(counter>1)
#dev.set(currentdev)
if(svalue(denseradio)=="Frequency")
densevalue<-TRUE
if(svalue(denseradio)=="Density")
densevalue<-FALSE


if(svalue(selectplot)=="Bond Angle Histogram")
{
myhplot<-function(a,b,c){
mydata<-A

laball<-c(a,b,c)
      laball<-as.data.frame(laball,ncol=1)
      rownames(laball)<-c('res1','res2','res3')
      laball<-na.omit(subset(laball,laball!='Any'))
      if (nrow(laball)>=1){    ## Specify more than (>=)1 residues use scatter3d best
      for (i in rownames(laball)){
           mydata<-mydata[which(as.character(mydata[,i])==rep(laball[i,1],nrow(mydata))),]
          }
     }
if(dim(mydata)<1 && svalue(selectres1) != "" && svalue(selectres2) != "" && svalue(selectres3) != "")
gmessage("No Data")
else
hist(mydata$BondAngles,breaks=svalue(anglebinslider),freq=densevalue, col="dark red",
border="lightslategray",
main="",xlim=c(50,180),
xlab=paste("Virtual bond angle for",a,b,c,"sequence"))
}
a<-svalue(selectres1)
b<-svalue(selectres2)
c<-svalue(selectres3)
if(a!="Any" | b!="Any" | c!="Any")
myhplot(a,b,c)
else
##aggregate plot of bond angles

hist(A$BondAngle,breaks=30000,border="lightslategray",
main="histogram of virtual bond angle")
}
if(svalue(selectplot)=="Residue 1-Residue 3 Distance Histogram")
{
mysaplot<-function(a,b,c){
mydata<-A
laball<-c(a,b,c)
      laball<-as.data.frame(laball,ncol=1)
      rownames(laball)<-c('res1','res2','res3')
      laball<-na.omit(subset(laball,laball!='Any'))
      if (nrow(laball)>=1){    ## Specify more than (>=)1 residues use scatter3d best
      for (i in rownames(laball)){
           mydata<-mydata[which(as.character(mydata[,i])==rep(laball[i,1],nrow(mydata))),]
          }
     }
if(dim(mydata)<1 && svalue(selectres1) != "" && svalue(selectres2) != "" && svalue(selectres3) != "" )
gmessage("No Data")
else     
hist(mydata$distance,breaks=svalue(anglebinslider),freq= densevalue,xlim=c(min(mydata$distance)-1,max(mydata$distance)+1),col="dark blue",
 border="lightslategray",main="",
xlab=paste("Distance between",a,c,"of sequence", a,b,c))

}
a<-svalue(selectres1)
b<-svalue(selectres2)
c<-svalue(selectres3)
if(a!="Any" | b!="Any" | c!="Any")
mysaplot(a,b,c)
else
##aggregate plot of 1-3 distances
hist(A$distance,breaks=30000,border="lightslategray",
main="histogram of res1-res3 distance")
}

if(svalue(selectplot)=="Angle-Distance Correlation")
{myscplot<-function(a,b,c){
mydata<-A
laball<-c(a,b,c)
      laball<-as.data.frame(laball,ncol=1)
      rownames(laball)<-c('res1','res2','res3')
      laball<-na.omit(subset(laball,laball!='Any'))
      if (nrow(laball)>=1){    ## Specify more than (>=)1 residues use scatter3d best
      for (i in rownames(laball)){
           mydata<-mydata[which(as.character(mydata[,i])==rep(laball[i,1],nrow(mydata))),]
          }
     }
if(dim(mydata)<1 && svalue(selectres1) != "" && svalue(selectres2) != "" && svalue(selectres3) != "")
gmessage("No Data")
else
plot(mydata$BondAngles~mydata$distance, main="Scatterplot of Bond Angle and 1-3 Distance")
abline(lm(mydata$BondAngles~mydata$distance))
}
a<-svalue(selectres1)
b<-svalue(selectres2)
c<-svalue(selectres3)
myscplot(a,b,c)
}
#currentdev<- dev.cur()
}

##GUI layout
selectres1<- gcombobox(c("","Any","ALA", "ARG", "ASN", "ASP","CYS", "GLN", "GLU", "GLY", "HIS", "ILE", "LEU", "LYS", "MET", "PHE", "PRO", "SER", "THR", "TRP", "TYR", "VAL")
, handler=updatePlot)
selectres2<- gcombobox(c("","Any","ALA", "ARG", "ASN", "ASP","CYS", "GLN", "GLU", "GLY", "HIS", "ILE", "LEU", "LYS", "MET", "PHE", "PRO", "SER", "THR", "TRP", "TYR", "VAL")
, handler=updatePlot)
selectres3<- gcombobox(c("","Any","ALA", "ARG", "ASN", "ASP","CYS", "GLN", "GLU", "GLY", "HIS", "ILE", "LEU", "LYS", "MET", "PHE", "PRO", "SER", "THR", "TRP", "TYR", "VAL")
, handler=updatePlot)
selectplot<-gradio(c("Bond Angle Histogram","Residue 1-Residue 3 Distance Histogram","Angle-Distance Correlation"), handler=updatePlot)
window <- gwindow("Bond Angle")
size(window)<-c(950,500)
BigGroup <- ggroup(horizontal=FALSE,cont=window)
BiggerGroup<-ggroup(cont=window)
group <- ggroup(horizontal=FALSE, container=BigGroup)
tmp <- gframe("Residue 1", container=group)
add(tmp, selectres1)
tmp <- gframe("Residue 2", container=group)
add(tmp, selectres2)
tmp <- gframe("Residue 3", container=group)
add(tmp, selectres3)
plotgroup<-ggroup(horizontal=FALSE, container=BigGroup)
tmp <- gframe("Select Plot", container=group)
add(tmp,selectplot)
binlabel<-glabel("Slider for Number of Bins", container=group)
anglebinslider<-gslider(from=1,to=200,by=2,value=20,container=group , handler=updatePlot)
denseradio<-gradio(c("Frequency","Density"), container=group, handler=updatePlot)
graphicsangle <- ggraphics()
add(BiggerGroup, graphicsangle)
}

