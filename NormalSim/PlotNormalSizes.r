rm(list=ls(all=TRUE));
load("answer.rdata");
max.n <- 1000;



for (Scenario in 1:2) {
  these.answers <- answers[answers[,"parameter.scenario"]==Scenario,];
  
  pdf(paste("Nor-",Scenario,"-Size.pdf",sep=""),height=6,width=6.5);
  #windows();
  par(mar = .8*c(5.5,6,4,1.2)+.1);
  par(mex= 1);
  par(las=1);
  cols <- c("black",gray(.55),"black",gray(.55),"black",gray(.20));   # AIC, ABIC, BIC, CAIC, AICc, full
  ltys <- c("dashed","dashed","solid","solid","dotted","dotdash");  # AIC, ABIC, BIC, CAIC, AICc, full
  lwds <- c( 2, 3, 2, 3, 2, 1 );
  par(cex.lab = 2.6);
  par(cex.axis = 2);
  par(cex.main = 2.6);
  par(mgp=c(2,0.1,0));
  if (Scenario==1) {main <- expression("With Diffuse DGM");}
  if (Scenario==2) {main <- expression("With Sparse DGM");}
  plot( these.answers[,"n"],
        these.answers[,"aic.size"],
        type="l",
        col=cols[1],
        lty=ltys[1],
        lwd=lwds[1],
        ylim=c(0,10),
        xlim=c(0,max.n),
        xlab=expression(italic(n)),
        main=main,
        ylab="Mean # Selected",
         axes=FALSE);
     axis(side=1,
          at=c(0,250,500,750,1000,1250,1500),
          mgp=c(1,1,0),
          pos=0,
          tck=.01
          );
     axis(2,
          at=c(0,2,4,6,8,10),
          mgp=c(1,0.3,0),
          pos=0,
          line=-1.1,
          tck=.01);
     lines(these.answers[,"n"],
          these.answers[,"bic.star.size"],
           type="l",
           col=cols[2],
           lty=ltys[2],
           lwd=lwds[2],
           ylim=c(0,1));
     lines(these.answers[,"n"],
        these.answers[,"bic.size"],
           type="l",
           col=cols[3],
           lty=ltys[3],
           lwd=lwds[3],
           ylim=c(0,1));
     lines(these.answers[,"n"],
        these.answers[,"caic.size"],
           type="l",
           col=cols[4],
           lty=ltys[4],
           lwd=lwds[4],
           ylim=c(0,1));
     lines(these.answers[,"n"],
        these.answers[,"aicc.size"],
           type="l",
           col=cols[5],
           lty=ltys[5],
           lwd=lwds[5],
           ylim=c(0,1));
#     lines(these.answers[,"n"],
#        these.answers[,"full.size"],
#           type="l",
#           col=cols[6],
#           lty=ltys[6],
#           lwd=lwds[6],
#           ylim=c(0,1));
           
  pdf(paste("Nor-",Scenario,"-mseB.pdf",sep=""),height=6,width=6.5);
  #windows();
  par(mar = .8*c(5.5,6,4,1.2)+.1);
  par(mex= 1);
  par(las=1);
  cols <- c("black",gray(.55),"black",gray(.55),"black",gray(.20));   # AIC, ABIC, BIC, CAIC, AICc, full
  ltys <- c("dashed","dashed","solid","solid","dotted","dotdash");  # AIC, ABIC, BIC, CAIC, AICc, full
  lwds <- c( 2, 3, 2, 3, 2, 1 );
  par(cex.lab = 2.6);
  par(cex.axis = 2);
  par(cex.main = 2.6);
  par(mgp=c(2,0.1,0));
  if (Scenario==1) {main <- expression("With Diffuse DGM");}
  if (Scenario==2) {main <- expression("With Sparse DGM");}
  plot( these.answers[,"n"],
        sqrt(these.answers[,"aic.mse1"]),
        type="l",
        col=cols[1],
        lty=ltys[1],
        lwd=lwds[1],
        ylim=c(0,.6),
        xlim=c(0,max.n),
        xlab=expression(italic(n)),
        main=main,
        ylab=expression(sqrt(MSE[beta])),
         axes=FALSE);
     axis(side=1,
          at=c(0,250,500,750,1000,1250,1500),
          mgp=c(1,1,0),
          pos=0,
          tck=.01
          );
     axis(2,
          at=c(0,.20,.40,.60),
          mgp=c(1,0.3,0),
          pos=0,
          cex.axis=1.5,
          cex.lab=2.5,
          line=-1.1,
          tck=.01);
     lines(these.answers[,"n"],
          sqrt(these.answers[,"bic.star.mse1"]),
           type="l",
           col=cols[2],
           lty=ltys[2],
           lwd=lwds[2],
           ylim=c(0,1));
     lines(these.answers[,"n"],
        sqrt(these.answers[,"bic.mse1"]),
           type="l",
           col=cols[3],
           lty=ltys[3],
           lwd=lwds[3],
           ylim=c(0,1));
     lines(these.answers[,"n"],
        sqrt(these.answers[,"caic.mse1"]),
           type="l",
           col=cols[4],
           lty=ltys[4],
           lwd=lwds[4],
           ylim=c(0,1));
     lines(these.answers[,"n"],
        sqrt(these.answers[,"aicc.mse1"]),
           type="l",
           col=cols[5],
           lty=ltys[5],
           lwd=lwds[5],
           ylim=c(0,1));
#     lines(these.answers[,"n"],
#        sqrt(these.answers[,"full.mse1"]),
#           type="l",
#           col=cols[6],
#           lty=ltys[6],
#           lwd=lwds[6],
#           ylim=c(0,1));

  pdf(paste("Nor-",Scenario,"-mseY.pdf",sep=""),height=6,width=6.5);
  #windows();
  par(mar = .8*c(5.5,6,4,1.2)+.1);
  par(mex= 1);
  par(las=1);
  cols <- c("black",gray(.55),"black",gray(.55),"black",gray(.30));   # AIC, ABIC, BIC, CAIC, AICc, full
  ltys <- c("dashed","dashed","solid","solid","dotted","dotdash");  # AIC, ABIC, BIC, CAIC, AICc, full
  lwds <- c( 2, 3, 2, 3, 2, 1 );
  par(cex.lab = 2.6);
  par(cex.axis = 2);
  par(cex.main = 2.6);
  par(mgp=c(2,0.1,0));
  if (Scenario==1) {main <- expression("With Diffuse DGM");}
  if (Scenario==2) {main <- expression("With Sparse DGM");}
  plot( these.answers[,"n"],
        sqrt(these.answers[,"aic.mse4"]),
        type="l",
        col=cols[1],
        lty=ltys[1],
        lwd=lwds[1],
        ylim=((Scenario==1)*c(.7,.85)+(Scenario==2)*c(.75,.90)),
        xlim=c(0,max.n),
        xlab=expression(italic(n)),
        main=main,
        ylab=expression(sqrt(MSE)),
         axes=FALSE);
     axis(side=1,
          at=c(0,250,500,750,1000,1250,1500),
          mgp=c(1,1,0),
          pos=ifelse(Scenario==1,0.7,.75),
          tck=.01
          );
     axis(2,
          at=((Scenario==1)*c(.70,.75,.80,.85)+(Scenario==2)*c(.75,.80,.85,.90)),
          mgp=c(1,0.3,0),
          pos=0,
          cex.axis=1.5,
          cex.lab=2.5,
          line=-1.1,
          tck=.01);
     lines(these.answers[,"n"],
          sqrt(these.answers[,"bic.star.mse4"]),
           type="l",
           col=cols[2],
           lty=ltys[2],
           lwd=lwds[2],
           ylim=c(0,1));
     lines(these.answers[,"n"],
        sqrt(these.answers[,"bic.mse4"]),
           type="l",
           col=cols[3],
           lty=ltys[3],
           lwd=lwds[3],
           ylim=c(0,1));
     lines(these.answers[,"n"],
        sqrt(these.answers[,"caic.mse4"]),
           type="l",
           col=cols[4],
           lty=ltys[4],
           lwd=lwds[4],
           ylim=c(0,1));
     lines(these.answers[,"n"],
        sqrt(these.answers[,"aicc.mse4"]),
           type="l",
           col=cols[5],
           lty=ltys[5],
           lwd=lwds[5],
           ylim=c(0,1));
#     lines(these.answers[,"n"],
#        sqrt(these.answers[,"full.mse4"]),
#           type="l",
#           col=cols[6],
#           lty=ltys[6],
#           lwd=lwds[6],
#           ylim=c(0,1));

pdf(paste("Nor-",Scenario,"-Legend.pdf",sep=""),height=6,width=6.5);
#windows();
par(mar = .75*c(.8,1,1,1)+.1);
plot(xlim=c(0,1),ylim=c(0,1),x=1,y=1,type="n",lty=0,bty="n",
     xaxt="n",yaxt="n",xlab="",ylab="",main="");
legend( x = -.03,
        y = 1 ,
        lty = ltys[c(1,2,5,3,4,6)],
        lwd = lwds[c(1,2,5,3,4,6)],
        col = cols[c(1,2,5,3,4,6)],
        legend= c( " AIC        ",
                   " ABIC       ",
                   " BIC        ",
                   " CAIC       ",
                   " AICc       ")[c(1,2,5,3,4)],
         y.intersp=1,
          x.intersp=.2,
        # horiz=TRUE,
         bty="n",
         cex = 1.2,
         ncol =6,
         #xjust=0.5,
         text.width=.1,
         yjust=1
      );
           
           
           
}

