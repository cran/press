callTAT<-function(){
#require("gWidgets")
options("guiToolkit"="RGtk2")
#library(ggplot2)
#library(Rcmdr)
#library(rgl)
#library(car)
TAT<-Torsion_Angle_Torsion

updatePlot <- function(h,...){

if(svalue(selectplot)=="2D Torsion-Angle-Torsion Correlation")
{
tat_scattergrid<-function(a,b,c,d,e){

      data<-TAT
      laball<-c(a,b,c,d,e)
      laball<-as.data.frame(laball,ncol=1)
      rownames(laball)<-c('res1','res2','res3','res4','res5')
      laball<-na.omit(subset(laball,laball!='Any'))
      if (nrow(laball)>=1){    ## Specify more than (>=)1 residues, then use scatter3d best
      for (i in rownames(laball)){
           data<-data[which(as.character(data[,i])==rep(laball[i,1],nrow(data))),]
          }
	if(dim(data)<1 && svalue(selectres1) != "" && svalue(selectres2) != "" && svalue(selectres3) != "" && svalue(selectres4) != "" && svalue(selectres5) != "")
	gmessage("No Data")
	else
      scatterplot.matrix(~torsion1+bondang+torsion2, data=data,main=paste("plot of residue sequence of",a,b,c,d,e))    
      }
}

a<-svalue(selectres1)
b<-svalue(selectres2)
c<-svalue(selectres3)
d<-svalue(selectres4)
e<-svalue(selectres5)
tat_scattergrid(a,b,c,d,e)
}
if(svalue(selectplot)=="3D Torsion-Angle-Torsion Correlation")
{tat_scatter3d<-function(a,b,c,d,e){
      data<-TAT
      laball<-c(a,b,c,d,e)
      laball<-as.data.frame(laball,ncol=1)
      rownames(laball)<-c('res1','res2','res3','res4','res5')
      laball<-na.omit(subset(laball,laball!='Any'))
      if (nrow(laball)>=1){    ## Specify more than (>=)1 residues, then use scatter3d best
      for (i in rownames(laball)){
           data<-data[which(as.character(data[,i])==rep(laball[i,1],nrow(data))),]
          }
	if(dim(data)<1 && svalue(selectres1) != "" && svalue(selectres2) != "" && svalue(selectres3) != "" && svalue(selectres4) != "" && svalue(selectres5) != "")
	gmessage("No Data")
	else
        scatter3d(data$torsion1,data$bondang,data$torsion2,fit="smooth",point.col="dark red",residuals=FALSE) 
      }
}

a<-svalue(selectres1)
b<-svalue(selectres2)
c<-svalue(selectres3)
d<-svalue(selectres4)
e<-svalue(selectres5)
tat_scatter3d(a,b,c,d,e)
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
selectres5<- gcombobox(c("","Any","ALA", "ARG", "ASN", "ASP","CYS", "GLN", "GLU", "GLY", "HIS", "ILE", "LEU", "LYS", "MET", "PHE", "PRO", "SER", "THR", "TRP", "TYR", "VAL")
, handler=updatePlot)
selectplot<-gradio(c("2D Torsion-Angle-Torsion Correlation","3D Torsion-Angle-Torsion Correlation"), handler=updatePlot)
window <- gwindow("TAT")
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
tmp <- gframe("Residue 5", container=group)
add(tmp, selectres5)
plotgroup<-ggroup(horizontal=FALSE, container=BigGroup)
tmp <- gframe("Select Plot", container=group)
add(tmp,selectplot)
add(BiggerGroup, ggraphics())
}
