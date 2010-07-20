callEval<-function(){
fileChoose <- function(action="print", text = "Select a file...",
type="open", ...) {
gfile(text=text, type=type, ..., action = action, handler =
function(h,...) {
do.call(h$action, list(h$file))
})
}
editfile<-function() 
{svalue(file)=fileChoose()
pdbfile<-svalue(file)
}

bond.eval<-function()
{
gconfirm("NOTE: Evaluating novel structures can take a LONG TIME.",
title="Warning: Long Load Time!",handler=function(h,...)confirmedbondeval())
}
confirmedbondeval<-function()
{galert("LOADING...", title="LOADING...",delay=5)
id<-svalue(name)
pdbfile<-svalue(file)
if (is.na(pdbfile)) pdb<-read.pdb(id) else pdb<-read.pdb(pdbfile)
pressbond(id,pdb)
}

angle.eval<-function()
{
gconfirm("NOTE: Evaluating novel structures can take a LONG TIME.",
title="Warning: Long Load Time!",handler=function(h,...)confirmedangleeval())
}
confirmedangleeval<-function()
{galert("LOADING...", title="LOADING...",delay=5)
id<-svalue(name)
pdbfile<-svalue(file)
if (is.na(pdbfile)) pdb<-read.pdb(id) else pdb<-read.pdb(pdbfile)
pressangle(id,pdb)
}

cont1.eval<-function()
{
gconfirm("NOTE: Evaluating novel structures can take a LONG TIME.",
title="Warning: Long Load Time!",handler=function(h,...)confirmedcont1eval())
}

confirmedcont1eval<-function()
{galert("LOADING...", title="LOADING...",delay=5)
id<-svalue(name)
pdbfile<-svalue(file)
if (is.na(pdbfile)) pdb<-read.pdb(id) else pdb<-read.pdb(pdbfile)
presscont1(id,pdb)
}

cont2.eval<-function()
{
gconfirm("NOTE: Evaluating novel structures can take a LONG TIME.",
title="Warning: Long Load Time!",handler=function(h,...)confirmedcont2eval())
}
confirmedcont2eval<-function()
{galert("LOADING...", title="LOADING...",delay=5)
id<-svalue(name)
pdbfile<-svalue(file)
if (is.na(pdbfile)) pdb<-read.pdb(id) else pdb<-read.pdb(pdbfile)
presscont2(id,pdb)
}
window<-gwindow("Select PDB file for evaluation")
name<-gedit(text="Enter name of pdb file")
file<-gedit(text="Enter path of pbd file",width=50)
vertgroup<-ggroup(horizonta=FALSE, cont=window)
tmp<-gframe("Name", cont=vertgroup)
add(tmp,name)
tmp<-gframe("File Path", cont=vertgroup)
add(tmp,file)
gbutton(text="Browse",handler= function(h,...) editfile(),cont=vertgroup)
addSpring(vertgroup)
gbutton(text="Evaluate Bonds Potentials",handler= function(h,...)bond.eval(),cont=vertgroup)
gbutton(text="Evaluate Angles Potentials",handler= function(h,...)angle.eval(),cont=vertgroup)
gbutton(text="Residue Level Ramachandran Plots a-t",handler= function(h,...)cont1.eval(),cont=vertgroup)
gbutton(text="Residue Level Ramachandran Plots b-t",handler= function(h,...)cont2.eval(),cont=vertgroup)

}



