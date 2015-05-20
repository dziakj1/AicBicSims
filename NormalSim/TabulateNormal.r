rm(list=ls(all=TRUE));
path <- "C:\\Users\\John\\Documents\\NormalSimMath\\";
mainname <- "NormalSim";
answers <- NULL;
n.values <- c(50,100,150,100*(2:15));
for (index.i in 1:length(n.values)) {
   for (index.j in 1:2) {
      datafilename <- paste(mainname,n.values[index.i],"-",index.j,".rdata",sep="");
      load(datafilename);
      answers <- rbind(answers,c(n=n.values[index.i],
                               parameter.scenario=index.j,
                               results));
   }
}
save.image("answer.rdata");