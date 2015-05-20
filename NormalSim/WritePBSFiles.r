path <- "C:\\Documents and Settings\\jjd264\\My Documents\\Paper-DziakCoffmanLanzaLi2011\\NormalSimMath\\";
mainname <- "NormalSimMath";
n <- c(50,100,150,100*(2:15));
walltime <- 10;
for (i in 1:length(n)) {
   for (j in 1:2) {
      print(paste("qsub ",mainname,"-",n[i],"-",j,".pbs",sep=""));
      pbsfilename <- paste(path,mainname,"-",n[i],"-",j,".pbs",sep="");
      rawfilename <- paste(mainname,"-",n[i],"-",j,sep="");
      # PBS file
      write(x="#PBS -q lionxf-mcenter",file=pbsfilename,append=FALSE);
      write(x="",file=pbsfilename,append=TRUE);
      write(x="#PBS -l nodes=1",file=pbsfilename,append=TRUE);
      write(x="",file=pbsfilename,append=TRUE);
      write(x="#PBS -l pmem=4gb",file=pbsfilename,append=TRUE);
      write(x="",file=pbsfilename,append=TRUE);
      write(x=paste("#PBS -l walltime=",walltime,":00:00",sep=""),file=pbsfilename,append=TRUE);
      write(x="",file=pbsfilename,append=TRUE);
      write(x="#PBS -j oe",file=pbsfilename,append=TRUE);
      write(x="",file=pbsfilename,append=TRUE);
      write(x="module load R/2.12.0",file=pbsfilename,append=TRUE);
      write(x="",file=pbsfilename,append=TRUE);
      write(x=paste("R --vanilla < ",rawfilename,".r > ",rawfilename,".txt",sep=""),file=pbsfilename,append=TRUE);
      write(x="",file=pbsfilename,append=TRUE);
   }
}
