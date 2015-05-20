rm(list=ls(all=TRUE));
path <- "C:\\Documents and Settings\\jjd264\\My Documents\\Paper-DziakCoffmanLanzaLi2010\\NormalSimMath\\";
n <- c(50,100,150,100*(2:15));
mainname <- "NormalSimMath";
for (i in 1:length(n)) {
   for (j in 1:2) {
      rfilename <- paste(path,mainname,"-",n[i],"-",j,".r",sep="");
      rawfilename <- paste(mainname,"-",n[i],"-",j,sep="");
      write(x="rm(list=ls(all=TRUE));",file=rfilename,append=FALSE);
      write(x="library(datasets); ",file=rfilename,append=TRUE);
      write(x=paste("n <- ",n[i],";",sep=""),file=rfilename,append=TRUE);
      write(x=paste("which.true.model <- ",j,";",sep=""),file=rfilename,append=TRUE);
      write(x="nReps <- 2000;",file=rfilename,append=TRUE);
      write(x='source("RestOfCode.r");',file=rfilename,append=TRUE);
   }
}
