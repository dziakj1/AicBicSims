rm(list=ls(all=TRUE));

###########################################################
pdf("FacSizes-3.pdf",height=6,width=6.5);
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
###########################################################

load("did-3.rdata");
n.values <- sizes;
err.from.true <- err2;
mean.apriori1.mse <- apply((err2[,,1]),1,mean);
mean.apriori2.mse <- apply((err2[,,2]),1,mean);
mean.apriori3.mse <- apply((err2[,,3]),1,mean);
mean.apriori4.mse <- apply((err2[,,4]),1,mean);
mean.apriori5.mse <- apply((err2[,,5]),1,mean);
plot( n.values,
      sqrt(mean.apriori1.mse),
      type="l",
      col=cols[1],
      lty=ltys[1],
      lwd=lwds[1],
      xlim=c(0,max(sizes)),
      ylim=c(0,.15),
      main=expression("With 3-factor DGM"),
      xlab=expression(italic(n)),
      ylab=expression(sqrt(MSE)),
      axes=FALSE);
   axis(side=1,
        at=c(0,100,200,300,400,500,600),
        mgp=c(1,1,0),
        pos=0,
        tck=.01
        );
   axis(2,
        at=(0:3)/20,
        mgp=c(.8,0.3,0),
        pos=0,
        line=-1.1,
        cex.axis=1,
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

###########################################################
pdf("FacSizes-4.pdf",height=6,width=6.5);
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
load("did-4.rdata");
n.values <- sizes;
err.from.true <- err2;
par(cex.lab = 2);
par(cex.axis = 2);
par(cex.main = 2.8);
mean.apriori1.mse <- apply((err2[,,1]),1,mean);
mean.apriori2.mse <- apply((err2[,,2]),1,mean);
mean.apriori3.mse <- apply((err2[,,3]),1,mean);
mean.apriori4.mse <- apply((err2[,,4]),1,mean);
mean.apriori5.mse <- apply((err2[,,5]),1,mean);
plot( n.values,
      sqrt(mean.apriori1.mse),
      type="l",
      col=cols[1],
      lty=ltys[1],
      lwd=lwds[1],
      xlim=c(0,max(sizes)),
      ylim=c(0,.15),
      main=expression("With 4-factor DGM"),
      xlab=expression(italic(n)),
      ylab=expression(sqrt(MSE)),axes=FALSE);
   axis(side=1,
        at=c(0,100,200,300,400,500,600),
        mgp=c(1,1,0),
        pos=0,
        tck=.01      ,
        );
   axis(2,
        at=(0:3)/20,
        mgp=c(.8,0.3,0),
        pos=0,
        line=-1.1,
        cex.axis=1,
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


##########################################################
pdf("FacSizes-Legend.pdf",height=1.1,width=6.5);
par(mar = .75*c(.8,1,1,1)+.1);
plot(xlim=c(0,1),ylim=c(0,1),x=1,y=1,type="n",lty=0,bty="n",
     xaxt="n",yaxt="n",xlab="",ylab="",main="");
legend( x = 0,
        y = 1 ,
        lty = ltys[1:5],
        lwd = lwds[1:5],
        col = cols[1:5],
        legend= paste(1:5,"-factor \n fitted",sep=""),
         bty="n",
         cex = .95,
         y.intersp=1.2,
         ncol =5#,
        # text.width=.1,
        # yjust=1
      );