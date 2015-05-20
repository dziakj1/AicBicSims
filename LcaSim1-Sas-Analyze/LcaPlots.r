rm(list=ls(all=TRUE));
for (K in 3:4) {
a <- read.table(paste("C:\\Documents and Settings\\jjd264\\My Documents\\Paper-DziakCoffmanLanzaLi2011\\LcaSim1-Sas-Analyze\\results",K,".txt",sep=""),header=TRUE);
max.n <- 1800;
if (K==3) {main <- expression("With 3-class DGM");}
if (K==4) {main <- expression("With 4-class DGM");}
a <- a[which(a[,"N"]==100 | a[,"N"]==200 | a[,"N"]==400 | a[,"N"]==600 |
             a[,"N"]==800 | a[,"N"]==1000 | a[,"N"]==1200 | a[,"N"]==1400 |
             a[,"N"]==1600 | a[,"N"]==1800),];
n.values <- a[,"N"];
mean.aic.over  <- a[, "AICOVER"];
mean.aic3.over  <- a[, "AICTHREEOVER"];
mean.bic.over  <- a[, "BICOVER"];
mean.caic.over <- a[,"CAICOVER"];
mean.abic.over <- a[,"ABICOVER"];
mean.aic.under  <- a[, "AICUNDER"];
mean.aic3.under  <- a[, "AICTHREEUNDER"];
mean.bic.under  <- a[, "BICUNDER"];
mean.caic.under <- a[,"CAICUNDER"];
mean.abic.under <- a[,"ABICUNDER"];
mean.aic.at  <- a[, "AICAT"];
mean.aic3.at  <- a[, "AICTHREEAT"];
mean.bic.at  <- a[, "BICAT"];
mean.caic.at <- a[,"CAICAT"];
mean.abic.at <- a[,"ABICAT"];
mean.aic.mse <- a[,"AICMSE"];
mean.aic3.mse <- a[,"AICTHREEMSE"];
mean.bic.mse <- a[,"BICMSE"];
mean.caic.mse <- a[,"CAICMSE"];
mean.abic.mse <- a[,"ABICMSE"];

pdf(paste("Lca-",K,"-Under.pdf",sep=""),height=6,width=6.5);
par(mar = .8*c(5.5,6,4,1.2)+.1);
par(mex= 1);
par(las=1);
cols <- c("black",gray(.55),"black",gray(.55),"black");   # AIC, ABIC, BIC, CAIC, AIC3
ltys <- c("dashed","dashed","solid","solid","dotted");  # AIC, ABIC, BIC, CAIC, AIC3
lwds <- c( 2, 3, 2, 3, 2 );
par(cex.lab = 2.8);
par(cex.axis = 2);
par(cex.main = 2.8);
par(mgp=c(2,0.1,0));
plot( n.values[(n.values<=max.n) & (n.values>=100)],
      100*mean.aic.under[(n.values<=max.n) & (n.values>=100)],
      type="l",
      col=cols[1],
      lty=ltys[1],
      lwd=lwds[1],
      xlim=c(0,max.n),
      ylim=c(0,100),
      xlab=expression(italic(n)),
      main=main,
      ylab="% Underfit",
      axes=FALSE);
   axis(side=1,
        at=c(0,200,400,600,800,1000,1200,1400,1600,1800),
        mgp=c(1,1,0),
        pos=0,
        tck=.01
        );
   axis(2,
        at=c(0,20,40,60,80,100),
        mgp=c(1,0.3,0),
        pos=0,
        line=-1.1,
        tck=.01);
   lines(n.values[(n.values<=max.n) & (n.values>=100)],
         100*mean.abic.under[(n.values<=max.n) & (n.values>=100)],
         type="l",
         col=cols[2],
         lty=ltys[2],
         lwd=lwds[2],
         ylim=c(0,1));
   lines(n.values[(n.values<=max.n) & (n.values>=100)],
         100*mean.bic.under[(n.values<=max.n) & (n.values>=100)],
         type="l",
         col=cols[3],
         lty=ltys[3],
         lwd=lwds[3],
         ylim=c(0,1));
   lines(n.values[(n.values<=max.n) & (n.values>=100)],
         100*mean.caic.under[(n.values<=max.n) & (n.values>=100)],
         type="l",
         col=cols[4],
         lty=ltys[4],
         lwd=lwds[4],
         ylim=c(0,1));
   lines(n.values[(n.values<=max.n) & (n.values>=100)],
         100*mean.aic3.under[(n.values<=max.n) & (n.values>=100)],
         type="l",
         col=cols[5],
         lty=ltys[5],
         lwd=lwds[5],
         ylim=c(0,1));

pdf(paste("Lca-",K,"-Over.pdf",sep=""),height=6,width=6.5);
par(mar = .8*c(5.5,6,4,1.2)+.1);
par(mex= 1);
par(las=1);
cols <- c("black",gray(.55),"black",gray(.55),"black");   # AIC, ABIC, BIC, CAIC, AIC3
ltys <- c("dashed","dashed","solid","solid","dotted");  # AIC, ABIC, BIC, CAIC, AIC3
lwds <- c( 2, 3, 2, 3, 2 );
par(cex.lab = 2.8);
par(cex.axis = 2);
par(cex.main = 2.8);
par(mgp=c(2,0.1,0));
plot( n.values[(n.values<=max.n) & (n.values>=100)],
      100*mean.aic.over[(n.values<=max.n) & (n.values>=100)],
      type="l",
      col=cols[1],
      lty=ltys[1],
      lwd=lwds[1],
      xlim=c(0,max.n),
      ylim=c(0,100),
      xlab=expression(italic(n)),
      main=main,
      ylab="% Overfit",
      axes=FALSE);
   axis(side=1,
        at=c(0,200,400,600,800,1000,1200,1400,1600,1800),
        mgp=c(1,1,0),
        pos=0,
        tck=.01
        );
   axis(2,
        at=c(0,20,40,60,80,100),
        mgp=c(1,0.3,0),
        pos=0,
        line=-1.1,
        tck=.01);
   lines(n.values[(n.values<=max.n) & (n.values>=100)],
         100*mean.abic.over[(n.values<=max.n) & (n.values>=100)],
         type="l",
         col=cols[2],
         lty=ltys[2],
         lwd=lwds[2],
         ylim=c(0,1));
   lines(n.values[(n.values<=max.n) & (n.values>=100)],
         100*mean.bic.over[(n.values<=max.n) & (n.values>=100)],
         type="l",
         col=cols[3],
         lty=ltys[3],
         lwd=lwds[3],
         ylim=c(0,1));
   lines(n.values[(n.values<=max.n) & (n.values>=100)],
         100*mean.caic.over[(n.values<=max.n) & (n.values>=100)],
         type="l",
         col=cols[4],
         lty=ltys[4],
         lwd=lwds[4],
         ylim=c(0,1));
   lines(n.values[(n.values<=max.n) & (n.values>=100)],
         100*mean.aic3.over[(n.values<=max.n) & (n.values>=100)],
         type="l",
         col=cols[5],
         lty=ltys[5],
         lwd=lwds[5],
         ylim=c(0,1));

pdf(paste("Lca-",K,"-At.pdf",sep=""),height=6,width=6.5);
par(mar = .8*c(5.5,6,4,1.2)+.1);
par(mex= 1);
par(las=1);
cols <- c("black",gray(.55),"black",gray(.55),"black");   # AIC, ABIC, BIC, CAIC, AIC3
ltys <- c("dashed","dashed","solid","solid","dotted");  # AIC, ABIC, BIC, CAIC, AIC3
lwds <- c( 2, 3, 2, 3, 2 );
par(cex.lab = 2.8);
par(cex.axis = 2);
par(cex.main = 2.8);
par(mgp=c(2,0.1,0));
plot( n.values[(n.values<=max.n) & (n.values>=100)],
      100*mean.aic.at[(n.values<=max.n) & (n.values>=100)],
      type="l",
      col=cols[1],
      lty=ltys[1],
      lwd=lwds[1],
      xlim=c(0,max.n),
      ylim=c(0,100),
      xlab=expression(italic(n)),
      main=main,
      ylab="% Correct Fit",
      axes=FALSE);
   axis(side=1,
        at=c(0,200,400,600,800,1000,1200,1400,1600,1800),
        mgp=c(1,1,0),
        pos=0,
        tck=.01
        );
   axis(2,
        at=c(0,20,40,60,80,100),
        mgp=c(1,0.3,0),
        pos=0,
        line=-1.1,
        tck=.01);
   lines(n.values[(n.values<=max.n) & (n.values>=100)],
         100*mean.abic.at[(n.values<=max.n) & (n.values>=100)],
         type="l",
         col=cols[2],
         lty=ltys[2],
         lwd=lwds[2],
         ylim=c(0,1));
   lines(n.values[(n.values<=max.n) & (n.values>=100)],
         100*mean.bic.at[(n.values<=max.n) & (n.values>=100)],
         type="l",
         col=cols[3],
         lty=ltys[3],
         lwd=lwds[3],
         ylim=c(0,1));
   lines(n.values[(n.values<=max.n) & (n.values>=100)],
         100*mean.caic.at[(n.values<=max.n) & (n.values>=100)],
         type="l",
         col=cols[4],
         lty=ltys[4],
         lwd=lwds[4],
         ylim=c(0,1));
   lines(n.values[(n.values<=max.n) & (n.values>=100)],
         100*mean.aic3.at[(n.values<=max.n) & (n.values>=100)],
         type="l",
         col=cols[5],
         lty=ltys[5],
         lwd=lwds[5],
         ylim=c(0,1));

pdf(paste("Lca-",K,"-mse.pdf",sep=""),height=6,width=6.5);
par(mar = .8*c(5.5,6,4,1.2)+.1);
par(mex= 1);
par(las=1);
cols <- c("black",gray(.55),"black",gray(.55),"black");   # AIC, ABIC, BIC, CAIC, AIC3
ltys <- c("dashed","dashed","solid","solid","dotted");  # AIC, ABIC, BIC, CAIC, AIC3
lwds <- c( 2, 3, 2, 3, 2 );
par(cex.lab = 2.8);
par(cex.axis = 2);
par(cex.main = 2.8);
par(mgp=c(2,0.1,0));
plot( n.values[(n.values<=max.n) & (n.values>=100)],
      sqrt(mean.aic.mse[(n.values<=max.n) & (n.values>=100)]),
      type="l",
      col=cols[1],
      lty=ltys[1],
      lwd=lwds[1],
      xlim=c(0,max.n),
      ylim=c(0,.02),
      xlab=expression(italic(n)),
      main=main,
      ylab=expression(sqrt(MSE)),
      axes=FALSE);
   axis(side=1,
        at=c(0,200,400,600,800,1000,1200,1400,1600,1800),
        mgp=c(1,1,0),
        pos=0,
        tck=.01
        );
   axis(2,
        at=c(0,0.005,0.010,0.015,0.020),
        mgp=c(1,0.3,0),
        cex.axis=1,
        pos=0,
        line=-1.1,
        tck=.01);
   lines(n.values[(n.values<=max.n) & (n.values>=100)],
         sqrt(mean.abic.mse[(n.values<=max.n) & (n.values>=100)]),
         type="l",
         col=cols[2],
         lty=ltys[2],
         lwd=lwds[2],
         ylim=c(0,1));
   lines(n.values[(n.values<=max.n) & (n.values>=100)],
         sqrt(mean.bic.mse[(n.values<=max.n) & (n.values>=100)]),
         type="l",
         col=cols[3],
         lty=ltys[3],
         lwd=lwds[3],
         ylim=c(0,1));
   lines(n.values[(n.values<=max.n) & (n.values>=100)],
         sqrt(mean.caic.mse[(n.values<=max.n) & (n.values>=100)]),
         type="l",
         col=cols[4],
         lty=ltys[4],
         lwd=lwds[4],
         ylim=c(0,1));
   lines(n.values[(n.values<=max.n) & (n.values>=100)],
         sqrt(mean.aic3.mse[(n.values<=max.n) & (n.values>=100)]),
         type="l",
         col=cols[5],
         lty=ltys[5],
         lwd=lwds[5],
         ylim=c(0,1));

pdf(paste("Lca-",K,"-Legend.pdf",sep=""),  height=1,width=6.5);
par(mar = .75*c(.8,1,1,1)+.1);
plot(xlim=c(0,1),ylim=c(0,1),x=1,y=1,type="n",lty=0,bty="n",
     xaxt="n",yaxt="n",xlab="",ylab="",main="");
legend( x = 0,
        y = 1 ,
        lty = ltys[c(1,2,5,3,4)],
        lwd = lwds[c(1,2,5,3,4)],
        col = cols[c(1,2,5,3,4)],
        legend= c( " AIC        ",
                   " ABIC       ",
                   " BIC        ",
                   " CAIC       ",
                   " AIC3       " )[c(1,2,5,3,4)],
         y.intersp=1,
          x.intersp=.5,
        # horiz=TRUE,
         bty="n",
         cex = 1.175,
         ncol =6,
         #xjust=0.5,
         text.width=.1,
         yjust=1
      );}