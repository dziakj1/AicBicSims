rm(list=ls(all=TRUE));
for (K in 3:4) {
load(paste("did-",K,".rdata",sep=""));
max.n <- 600;

AIC3s <- -2*logliks.matrix + 3*num.params.matrix;
picks.AIC3 <- apply(AIC3s,c(1,2),which.min);
over.AIC3 <- picks.AIC3 > true.number.of.factors;
under.AIC3 <- picks.AIC3 < true.number.of.factors;
at.AIC3 <- picks.AIC3 == true.number.of.factors;
if (K==3) {main <- expression("With 3-factor DGM");}
if (K==4) {main <- expression("With 4-factor DGM");}

mean.aic.under <-  apply(under.AIC,1,mean);
mean.abic.under <-  apply(under.AdjBIC,1,mean);
mean.bic.under <-  apply(under.BIC,1,mean);
mean.caic.under <-  apply(under.CAIC,1,mean);
mean.aic3.under <-  apply(under.AIC3,1,mean);
mean.aic.at <-  apply(at.AIC,1,mean);
mean.abic.at <-  apply(at.AdjBIC,1,mean);
mean.bic.at <-  apply(at.BIC,1,mean);
mean.caic.at <-  apply(at.CAIC,1,mean);
mean.aic3.at <-  apply(at.AIC3,1,mean);
mean.aic.over <-  apply(over.AIC,1,mean);
mean.abic.over <-  apply(over.AdjBIC,1,mean);
mean.bic.over <-  apply(over.BIC,1,mean);
mean.caic.over <-  apply(over.CAIC,1,mean);
mean.aic3.over <-  apply(over.AIC3,1,mean);

pdf(paste("Fac-",K,"-Under.pdf",sep=""),height=6,width=6.5);
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
plot( sizes,
      100*mean.aic.under,
      type="l",
      col=cols[1],
      lty=ltys[1],
      lwd=lwds[1],
      ylim=c(0,100),
      xlim=c(0,max.n),
      xlab=expression(italic(n)),
      main=main,
      ylab="% Underfit",
       axes=FALSE);
   axis(side=1,
        at=c(0,100,200,300,400,500,600),
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
   lines(sizes,
         100*mean.abic.under,
         type="l",
         col=cols[2],
         lty=ltys[2],
         lwd=lwds[2],
         ylim=c(0,1));
   lines(sizes,
         100*mean.bic.under,
         type="l",
         col=cols[3],
         lty=ltys[3],
         lwd=lwds[3],
         ylim=c(0,1));
   lines(sizes,
         100*mean.caic.under,
         type="l",
         col=cols[4],
         lty=ltys[4],
         lwd=lwds[4],
         ylim=c(0,1));
   lines(sizes,
         100*mean.aic3.under,
         type="l",
         col=cols[5],
         lty=ltys[5],
         lwd=lwds[5],
         ylim=c(0,1));

pdf(paste("Fac-",K,"-Over.pdf",sep=""),height=6,width=6.5);
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
plot( sizes,
      100*mean.aic.over,
      type="l",
      col=cols[1],
      lty=ltys[1],
      lwd=lwds[1],
      ylim=c(0,100),
      xlim=c(0,max.n),
      xlab=expression(italic(n)),
      main=main,
      ylab="% Overfit",
       axes=FALSE);
   axis(side=1,
        at=c(0,100,200,300,400,500,600),
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
   lines(sizes,
         100*mean.abic.over,
         type="l",
         col=cols[2],
         lty=ltys[2],
         lwd=lwds[2],
         ylim=c(0,1));
   lines(sizes,
         100*mean.bic.over,
         type="l",
         col=cols[3],
         lty=ltys[3],
         lwd=lwds[3],
         ylim=c(0,1));
   lines(sizes,
         100*mean.caic.over,
         type="l",
         col=cols[4],
         lty=ltys[4],
         lwd=lwds[4],
         ylim=c(0,1));
   lines(sizes,
         100*mean.aic3.over,
         type="l",
         col=cols[5],
         lty=ltys[5],
         lwd=lwds[5],
         ylim=c(0,1));


pdf(paste("Fac-",K,"-At.pdf",sep=""),height=6,width=6.5);
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
plot( sizes,
      100*mean.aic.at,
      type="l",
      col=cols[1],
      lty=ltys[1],
      lwd=lwds[1],
      ylim=c(0,100),
      xlim=c(0,max.n),
      xlab=expression(italic(n)),
      main=main,
      ylab="% Correct Fit",
       axes=FALSE);
   axis(side=1,
        at=c(0,100,200,300,400,500,600),
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
   lines(sizes,
         100*mean.abic.at,
         type="l",
         col=cols[2],
         lty=ltys[2],
         lwd=lwds[2],
         ylim=c(0,1));
   lines(sizes,
         100*mean.bic.at,
         type="l",
         col=cols[3],
         lty=ltys[3],
         lwd=lwds[3],
         ylim=c(0,1));
   lines(sizes,
         100*mean.caic.at,
         type="l",
         col=cols[4],
         lty=ltys[4],
         lwd=lwds[4],
         ylim=c(0,1));
   lines(sizes,
         100*mean.aic3.at,
         type="l",
         col=cols[5],
         lty=ltys[5],
         lwd=lwds[5],
         ylim=c(0,1));

a1 <- as.vector(err1);
a2 <- as.vector(err2);
a3 <- as.vector(err3);
a4 <- as.vector(err4);
cor(cbind(a1,a2,a3,a4));
err.from.observed <- err1;
err.from.true <- err2;
aic.mse <-  matrix(0,length(sizes),nrep);
abic.mse <- matrix(0,length(sizes),nrep);
bic.mse <-  matrix(0,length(sizes),nrep);
caic.mse <- matrix(0,length(sizes),nrep);
aic3.mse <- matrix(0,length(sizes),nrep);
for (i in 1:length(sizes)) {
   for (j in 1:nrep) {
      aic.mse[i,j]  <- (err.from.true[i,j,picks.AIC[i,j]]);
      abic.mse[i,j] <- (err.from.true[i,j,picks.AdjBIC[i,j]]);
      bic.mse[i,j]  <- (err.from.true[i,j,picks.BIC[i,j]]);
      caic.mse[i,j] <- (err.from.true[i,j,picks.CAIC[i,j]]);
      aic3.mse[i,j] <- (err.from.true[i,j,picks.AIC3[i,j]]);
   }
}
mean.aic.mse <- apply(aic.mse,1,mean);
mean.abic.mse <- apply(abic.mse,1,mean);
mean.bic.mse <- apply(bic.mse,1,mean);
mean.caic.mse <- apply(caic.mse,1,mean);
mean.aic3.mse <- apply(aic3.mse,1,mean);

pdf(paste("Fac-",K,"-mse.pdf",sep=""),height=6,width=6.5);
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
plot( sizes,
      sqrt(mean.aic.mse),
      type="l",
      col=cols[1],
      lty=ltys[1],
      lwd=lwds[1],
      ylim=c(0,.15),
      xlim=c(0,max.n),
      main=main,
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
        at=c(0,.05,.10,.15),
        mgp=c(2,0.3,0),
        pos=0,
        line=-1.1,
        cex.axis=1.2,
        cex.lab=1.5,
        tck=.01);
   lines(sizes,
         sqrt(mean.abic.mse),
         type="l",
         col=cols[2],
         lty=ltys[2],
         lwd=lwds[2],
         ylim=c(0,1));
   lines(sizes,
         sqrt(mean.bic.mse),
         type="l",
         col=cols[3],
         lty=ltys[3],
         lwd=lwds[3],
         ylim=c(0,1));
   lines(sizes,
         sqrt(mean.caic.mse),
         type="l",
         col=cols[4],
         lty=ltys[4],
         lwd=lwds[4],
         ylim=c(0,1));
   lines(sizes,
         sqrt(mean.aic3.mse),
         type="l",
         col=cols[5],
         lty=ltys[5],
         lwd=lwds[5],
         ylim=c(0,1));


pdf(paste("Fac-",K,"-Legend.pdf",sep=""),  height=1,width=6.5);
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
      );
}