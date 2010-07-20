press<-function(){
#source("callbond.r")
#source("callangle.r")
#source("calltorsion.r")
#source("calltat.r")
require("gWidgets")
options("guiToolkit"="RGtk2")

window <- gwindow("P.R.E.S.S.", width=520)

tutorial <- function(...) gmessage("This will open a basic tutorial of P.R.E.S.S.")
contact <- function(...) gmessage("Mantainer:\nYuanyuan Huang sunnyuan.h@gmail.com\n\nOther Developers:\nStephen Bonett sbonett@gmail.com\nDr. Zhijun Wu zhijun@iastate.edu",title="Contact Information")
devgroups <- function(...) gmessage("development groups")

helpactions<-list(Tutorial=gaction("Tutorial", icon="help", handler=tutorial),
		  Contact=gaction("Contact", handler=contact),
		  DevelopmentGroups=gaction("Development Groups", handler=devgroups))

menubarlist=list(File=list(quit=gaction("Quit", icon="quit", handler=function(...) dispose(window))),Help=helpactions)
gmenu(menubarlist, cont=window)


BigGroup <- ggroup(horizontal=FALSE,cont=window)
topgroup <- ggroup(cont=BigGroup, expand=TRUE)
middlegroup<- ggroup(cont=BigGroup, expand =TRUE)
bottomgroup<- ggroup(cont=BigGroup, expand=TRUE)


button1<-gbutton("Bond Distance - Two Residues", cont=topgroup, handler =function(h,...) callBond())
font(button1) <- c(color="blue")
size(button1) <- c(170,80)
button2<-gbutton("Bond Angle - Three Residues", cont=topgroup, handler = function(h,...) callAngle())
font(button2) <- c(color="blue")
size(button2) <- c(170,80)
button3<-gbutton("Torsion, ATA - Four Residues  ", cont=topgroup, handler = function(h,...) callTorsion())
font(button3) <- c(color="blue")
size(button3) <- c(170,80)
title<- glabel(text=" P.R.E.S.S.", cont=middlegroup)
font(title)<-c(color="red",size=90)


  
button4<-gbutton("         TAT - Five Residues        ", cont=bottomgroup, handler = function(h,...) callTAT())
font(button4) <- c(color="blue")
size(button4) <- c(170,80)
button5<-gbutton("         Structural Analysis        ", cont=bottomgroup, handler = function(h,...) callEval())
font(button5) <- c(color="blue")
size(button5) <- c(170,80)
button6<-gbutton("                  Info                      ", cont=bottomgroup, handler = function(h,...) gmessage("P.R.E.S.S. Version 1.0 \nCopywrite 2010 Yuanyuan Huang, Stephen Bonett, Zhijun Wu",title="P.R.E.S.S. Info"))
font(button6) <- c(color="blue")
size(button6) <- c(170,80)

}




