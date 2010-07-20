callTorsion<-function(){
#require("gWidgets")
options("guiToolkit"="RGtk2")
#library(ggplot2)
#library(Rcmdr)
#library(rgl)
#library(car)
#ATA<-read.csv("p:/BioMath/pdb/Angle_Torsion_Angle.csv",header=T)
#T<-read.csv("p:/BioMath/pdb/Torsion_Distance.csv",header=T)
ATA<-Angle_Torsion_Angle
T<-Torsion_Distance

updatePlot <- function(h,...){

if(svalue(denseradio)=="Frequency")
densevalue<-TRUE
if(svalue(denseradio)=="Density")
densevalue<-FALSE

if(svalue(selectplot)=="Torsion Histogram")
{
mytplot<-function(a,b,c,d){
mydata<-T
laball<-c(a,b,c,d)
      laball<-as.data.frame(laball,ncol=1)
      rownames(laball)<-c('res1','res2','res3','res4')
      laball<-na.omit(subset(laball,laball!='Any'))
      if (nrow(laball)>=1){    ## Specify more than (>=)1 residues use scatter3d best
      for (i in rownames(laball)){
           mydata<-mydata[which(as.character(mydata[,i])==rep(laball[i,1],nrow(mydata))),]
          }
     }
if(dim(mydata)<1 && svalue(selectres1) != "" && svalue(selectres2) != "" && svalue(selectres3) != "" && svalue(selectres4) != "")
gmessage("No Data")
else { 
low<-round(min(mydata$Torsion)-0.05,1)
high<-round(max(mydata$Torsion)+0.05,1)
hist(mydata$Torsion,breaks=svalue(torsionbinslider),freq=densevalue,border="lightslategray",
xlim=c(low, high),
main=paste("histogram of torsional angle for",a,b,c,d,"sequence"))
}
}
a<-svalue(selectres1)
b<-svalue(selectres2)
c<-svalue(selectres3)
d<-svalue(selectres4)
if(a!="Any" | b!="Any" | c!="Any" | d!="Any")
mytplot(a,b,c,d)
}
if(svalue(selectplot)=="3D Angle-Torsion-Angle Correlation")
{
ata_scatter<-function(a,b,c,d){
      data<-ATA
      laball<-c(a,b,c,d)
      laball<-as.data.frame(laball,ncol=1)
      rownames(laball)<-c('res1','res2','res3','res4')
      laball<-na.omit(subset(laball,laball!='Any'))
      if (nrow(laball)>=1){    ## Specify more than (>=)1 residues use scatter3d best
      for (i in rownames(laball)){
           data<-data[which(as.character(data[,i])==rep(laball[i,1],nrow(data))),]
          }
	if(dim(data)<1 && svalue(selectres1) != "" && svalue(selectres2) != "" && svalue(selectres3) != "" && svalue(selectres4) != "")
	gmessage("No Data")
	else
      scatter3d(data$angle1,data$torsional,data$angle2,fit="smooth",point.col="dark red",residuals=FALSE)
      }
      if (nrow(laball)==0) {   ## With no specified residue names use 2d plot
      scatterplot.matrix(~angle1+torsional+angle2, data=data,main=paste("plot of residue sequence of",a,b,c,d))
      }
}
a<-svalue(selectres1)
b<-svalue(selectres2)
c<-svalue(selectres3)
d<-svalue(selectres4)
ata_scatter(a,b,c,d)
}

if(svalue(selectplot)=="2D Angle-Torsion-Angle Correlation")
{ata_scattergrid<-function(a,b,c,d){
      data<-ATA
      laball<-c(a,b,c,d)
      laball<-as.data.frame(laball,ncol=1)
      rownames(laball)<-c('res1','res2','res3','res4')
      laball<-na.omit(subset(laball,laball!='Any'))
      if (nrow(laball)>=1){    ## Specify more than (>=)1 residues use scatter3d best
      for (i in rownames(laball)){
           data<-data[which(as.character(data[,i])==rep(laball[i,1],nrow(data))),]
          }
	if(dim(data)<=1 && svalue(selectres1) != "" && svalue(selectres2) != "" && svalue(selectres3) != "" && svalue(selectres4) != "")
	gmessage("No Data")
	else
	scatterplot.matrix(~angle1+torsional+angle2, data=data,main=paste("plot of residue sequence of",a,b,c,d)) }

}
a<-svalue(selectres1)
b<-svalue(selectres2)
c<-svalue(selectres3)
d<-svalue(selectres4)
ata_scattergrid(a,b,c,d)
}
}


selectres1<- gcombobox(c("","Any","ALA", "ARG", "ASN", "ASP","CYS", "GLN", "GLU", "GLY", "HIS", "ILE", "LEU", "LYS", "MET", "PHE", "PRO", "SER", "THR", "TRP", "TYR", "VAL")
, handler=updatePlot)
selectres2<- gcombobox(c("","Any","ALA", "ARG", "ASN", "ASP","CYS", "GLN", "GLU", "GLY", "HIS", "ILE", "LEU", "LYS", "MET", "PHE", "PRO", "SER", "THR", "TRP", "TYR", "VAL")
, handler=updatePlot)
selectres3<- gcombobox(c("","Any","ALA", "ARG", "ASN", "ASP","CYS", "GLN", "GLU", "GLY", "HIS", "ILE", "LEU", "LYS", "MET", "PHE", "PRO", "SER", "THR", "TRP", "TYR", "VAL")
, handler=updatePlot)
selectres4<- gcombobox(c("","Any","ALA", "ARG", "ASN", "ASP","CYS", "GLN", "GLU", "GLY", "HIS", "ILE", "LEU", "LYS", "MET", "PHE", "PRO", "SER", "THR", "TRP", "TYR", "VAL")
, handler=updatePlot)
selectplot<-gradio(c("Torsion Histogram", "3D Angle-Torsion-Angle Correlation", "2D Angle-Torsion-Angle Correlation"), handler=updatePlot)
window <- gwindow("Torsion and ATA")
size(window)<-c(1000,600)
BigGroup <- ggroup(horizontal=FALSE,cont=window)
BiggerGroup<-ggroup(cont=window)
group <- ggroup(horizontal=FALSE, container=BigGroup)
tmp <- gframe("Residue 1", container=group)
add(tmp, selectres1)
tmp <- gframe("Residue 2", container=group)
add(tmp, selectres2)
tmp <- gframe("Residue 3", container=group)
add(tmp, selectres3)
tmp <- gframe("Residue 4", container=group)
add(tmp, selectres4)
plotgroup<-ggroup(horizontal=FALSE, container=BigGroup)
tmp <- gframe("Select Plot", container=group)
add(tmp,selectplot)
binlabel<-glabel("Slider for Number of Bins", container=group)
torsionbinslider<-gslider(from=1,to=100,by=2,value=20,container=group , handler=updatePlot)
denseradio<-gradio(c("Frequency","Density"), container=group, handler=updatePlot)
add(BiggerGroup, ggraphics())
}

