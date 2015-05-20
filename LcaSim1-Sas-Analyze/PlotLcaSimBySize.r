rm(list=ls(all=TRUE));

###########################################################
pdf("LcaSizes-3.pdf",height=6,width=6.5);
par(mar=c(3,4,4,1));
par(tck=.01);
lwds <- c( 2, 3, 2, 3, 2 );
par(cex.lab = 2.8);
par(cex.axis = 2);
par(cex.main = 2.8);
par(mgp=c(2,0.1,0));
par(mex= 1);
par(las=1);
cols <- c("black",gray(.55),"black",gray(.55),"black",gray(.55));
ltys <- c("dashed","dashed","solid","solid","dotted","dotted");
lwds <- c( 2, 3, 2, 3, 2, 3 );
par(cex.lab = 2);
par(cex.axis = 2);
par(cex.main = 2.8);
a <- read.table("meanmse3.txt",header=TRUE);
a <- a[which(a[,"n"]==100 | a[,"n"]==200 | a[,"n"]==400 | a[,"n"]==600 |
             a[,"n"]==800 | a[,"n"]==1000 | a[,"n"]==1200 | a[,"n"]==1400 |
             a[,"n"]==1600 | a[,"n"]==1800),];
n.values <- a[(a[,"n"]>=100)&(a[,"n"]<=1000),"n"];
mean.apriori1.mse <- a[which((n.values>=100)&(n.values<=1000)),"mse1"];
mean.apriori2.mse <- a[which((n.values>=100)&(n.values<=1000)),"mse2"];
mean.apriori3.mse <- a[which((n.values>=100)&(n.values<=1000)),"mse3"];
mean.apriori4.mse <- a[which((n.values>=100)&(n.values<=1000)),"mse4"];
mean.apriori5.mse <- a[which((n.values>=100)&(n.values<=1000)),"mse5"];
mean.apriori6.mse <- a[which((n.values>=100)&(n.values<=1000)),"mse6"];
plot( n.values,
      sqrt(mean.apriori1.mse),
      type="l",
      col=cols[1],
      lty=ltys[1],
      lwd=lwds[1],
      xlim=c(0,1000),
      ylim=c(0,.025),
      xlab=expression(italic(n)),
      main=expression("With 3-class DGM"),
      ylab=expression(sqrt(MSE)),axes=FALSE );
   axis(side=1,
        at=c(0,200,400,600,800,1000),
        mgp=c(1,1,0),
        pos=0,
        tck=.01
        );
   axis(2,
        at=c(0,.005,.01,.015,.02,.025),
        mgp=c(.8,0.3,0),
        pos=0,
        line=-1.1,
        cex.axis=1.2,
        tck=.01);
lines(n.values,
      sqrt(mean.apriori2.mse),
      col=cols[2],
      lty=ltys[2],
      lwd=lwds[2] );
lines(n.values,
      sqrt(mean.apriori3.mse),
      col=cols[3],
      lty=ltys[3],
      lwd=lwds[3] );
lines(n.values,
      sqrt(mean.apriori4.mse),
      col=cols[4],
      lty=ltys[4],
      lwd=lwds[4] );
lines(n.values,
      sqrt(mean.apriori5.mse),
      col=cols[5],
      lty=ltys[5],
      lwd=lwds[5] );
#lines(n.values,
#      sqrt(mean.apriori6.mse),
#      col=cols[6],
#      lty=ltys[6],
#      lwd=lwds[6] );

##########################################################
pdf("LcaSizes-4.pdf",height=6,width=6.5);
par(mar=c(3,4,4,1));
par(tck=.01);
lwds <- c( 2, 3, 2, 3, 2 );
par(cex.lab = 2.8);
par(cex.axis = 2);
par(cex.main = 2.8);
par(mgp=c(2,0.1,0));
par(mex= 1);
par(las=1);
cols <- c("black",gray(.55),"black",gray(.55),"black",gray(.55));
ltys <- c("dashed","dashed","solid","solid","dotted","dotted");
lwds <- c( 2, 3, 2, 3, 2, 3 );

par(cex.lab = 2);
par(cex.axis = 2);
par(cex.main = 2.8);
a <- read.table("meanmse4.txt",header=TRUE);
a <- a[which(a[,"n"]==100 | a[,"n"]==200 | a[,"n"]==400 | a[,"n"]==600 |
             a[,"n"]==800 | a[,"n"]==1000 | a[,"n"]==1200 | a[,"n"]==1400 |
             a[,"n"]==1600 | a[,"n"]==1800),];
n.values <- a[a[,"n"]<=1000,"n"];
mean.apriori1.mse <- a[which(n.values<=1000),"mse1"];
mean.apriori2.mse <- a[which(n.values<=1000),"mse2"];
mean.apriori3.mse <- a[which(n.values<=1000),"mse3"];
mean.apriori4.mse <- a[which(n.values<=1000),"mse4"];
mean.apriori5.mse <- a[which(n.values<=1000),"mse5"];
mean.apriori6.mse <- a[which(n.values<=1000),"mse6"];
plot( n.values,
      sqrt(mean.apriori1.mse),
      type="l",
      col=cols[1],
      lty=ltys[1],
      lwd=lwds[1],
      xlim=c(0,1000),
      ylim=c(0,.025),
      xlab=expression(italic(n)),
      main=expression("With 4-class DGM"),
      ylab=expression(sqrt(MSE)),axes=FALSE );
   axis(side=1,
        at=c(0,200,400,600,800,1000),
        mgp=c(1,1,0),
        pos=0,
        tck=.01
        );
   axis(2,
        at=c(0,.005,.01,.015,.02,.025),
        mgp=c(.8,0.3,0),
        pos=0,
        line=-1.1,
        cex.axis=1.2,
        tck=.01);
lines(n.values,
      sqrt(mean.apriori2.mse),
      col=cols[2],
      lty=ltys[2],
      lwd=lwds[2] );
lines(n.values,
      sqrt(mean.apriori3.mse),
      col=cols[3],
      lty=ltys[3],
      lwd=lwds[3] );
lines(n.values,
      sqrt(mean.apriori4.mse),
      col=cols[4],
      lty=ltys[4],
      lwd=lwds[4] );
lines(n.values,
      sqrt(mean.apriori5.mse),
      col=cols[5],
      lty=ltys[5],
      lwd=lwds[5] );
#lines(n.values,
#      sqrt(mean.apriori6.mse),
#      col=cols[6],
#      lty=ltys[6],
#      lwd=lwds[6] );
#

##########################################################
pdf("LcaSizes-Legend.pdf",height=1.1,width=6.5);
par(mar = .75*c(.8,1,1,1)+.1);
plot(xlim=c(0,1),ylim=c(0,1),x=1,y=1,type="n",lty=0,bty="n",
     xaxt="n",yaxt="n",xlab="",ylab="",main="");
legend( x = 0,
        y = 1 ,
        lty = ltys[1:5],
        lwd = lwds[1:5],
        col = cols[1:5],
        legend= paste(1:5,"-class \n fitted",sep=""),
         bty="n",
         cex = .95,
         y.intersp=1.2,
         ncol =5,
        # text.width=.1,
        # yjust=1
      );