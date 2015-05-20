time1 <- Sys.time();
interesting.vars <- c( "GeneralInformation",
                     "PargraphComprehension",
                     "WordClassification",
                     "WordMeaning",
                     "Addition",
                     "Code",
                     "CountingDots",
                     "Deduction",          
                     "ProblemReasoning",    
                     "SeriesCompletion", 
                     "ArithmeticProblems");
pop.corr <- Harman74.cor$cov[interesting.vars,interesting.vars];
library(mvtnorm);
p <- nrow(pop.corr);
nPredAvailable <- p-1;
x.and.y.random <- rmvnorm(n=100000,mean=rep(0,p),sigma=pop.corr);
x <- x.and.y.random[,1:nPredAvailable];
colnames(x) <- interesting.vars[-p];
y <- x.and.y.random[,p];
model1 <- lm(y~x+0);
mu1 <- model1$fitted.values;
true.beta.full <- solve(pop.corr[1:nPredAvailable,1:nPredAvailable],pop.corr[1:nPredAvailable,p]);
mu1star <- x%*% true.beta.full;
cor(mu1,mu1star);
print(max(abs(model1$coef-true.beta.full)));
in.sparse <- c(2,5);
model2 <- lm(y~x[,in.sparse]+0);
mu2 <- model2$fitted.values;
mu2star <- x[,in.sparse]%*%solve(pop.corr[in.sparse,in.sparse],pop.corr[in.sparse,p]);
true.beta.sparse <- rep(0,length(true.beta.full));
true.beta.sparse[in.sparse] <- solve(pop.corr[in.sparse,in.sparse],pop.corr[in.sparse,p]);
cor(mu2,mu2star);
print(cbind(true.beta.full,true.beta.sparse));
print(interesting.vars[in.sparse]);
p <- nrow(pop.corr);
if (which.true.model==1) {
   true.beta <- true.beta.full;
}
if (which.true.model==2) {
   true.beta <- true.beta.sparse;
} 
true.subset <- abs(true.beta)>0;
subsets <- matrix(0,2^nPredAvailable,nPredAvailable);
for (k in 1:nPredAvailable) {
   subsets[,k] <- rep(c(1,0),each=2^(nPredAvailable-k),times=2^(k-1));
}                    
n.subsets <- nrow(subsets);
mse <- array(NA,c(n.subsets,nReps,5));
betas <- array(NA,c(n.subsets,nReps,p));
true.pos <- matrix(NA,n.subsets,nReps);
false.pos <- matrix(NA,n.subsets,nReps);
true.neg <- matrix(NA,n.subsets,nReps);
false.neg <- matrix(NA,n.subsets,nReps);
log.lik <- matrix(NA,n.subsets,nReps);
adjr2 <- matrix(NA,n.subsets,nReps);
nPredIn <- matrix(NA,n.subsets,nReps);
for (Rep in 1:nReps) {
   N <- 10000;
   big.u <- matrix(rnorm(nPredAvailable*N),ncol=nPredAvailable);
   big.x <- big.u %*% chol(pop.corr[1:nPredAvailable,1:nPredAvailable]);
   big.mu <- big.x%*%true.beta;
   big.y <- big.mu+sqrt(1-var(big.mu))*rnorm(N);
   u <- matrix(rnorm(nPredAvailable*n),ncol=nPredAvailable);
   x <- u %*% chol(pop.corr[1:nPredAvailable,1:nPredAvailable]);
   mu <- x%*%true.beta;
   y <- mu+sqrt(1-var(mu))*rnorm(n);
   for (subset.index in 1:n.subsets) {
      print(c(Rep,subset.index));
      this.subset <- subsets[subset.index,];
      if (sum(subsets[subset.index,])>0) {
        this.x <- x[,which(subsets[subset.index,]>0)];
        this.model <- lm(y~this.x);
      } else {
        this.model <- lm(y~1);
      } 
      this.log.lik <- logLik(this.model);
      this.beta <- rep(0,p);
      this.beta[which(c(1,subsets[subset.index,])>0)] <- this.model$coef;
      betas[subset.index,Rep,] <- this.beta;
      big.mu.hat <- cbind(1,big.x)%*%this.beta;
      mse[subset.index,Rep,1] <- t(this.beta-c(0,true.beta))%*%(this.beta-c(0,true.beta));
      EXTX = rbind(c(1,rep(0,nPredAvailable)),cbind(0,pop.corr[-p,-p]));
      mse[subset.index,Rep,2] <- t(this.beta-c(0,true.beta))%*%EXTX%*%(this.beta-c(0,true.beta));
      mse[subset.index,Rep,3] <- mean((big.mu.hat-big.mu)^2);
      mse[subset.index,Rep,4] <- mean((big.y-big.mu.hat)^2);
      mse[subset.index,Rep,5] <- mean((this.model$residuals)^2);
      true.pos[subset.index,Rep] <- sum(abs(this.beta[-1])>0 & abs(true.beta)>0);
      false.pos[subset.index,Rep] <- sum(abs(this.beta[-1])>0 & abs(true.beta)==0);
      true.neg[subset.index,Rep] <- sum(abs(this.beta[-1])==0 & abs(true.beta)==0);
      false.neg[subset.index,Rep] <- sum(abs(this.beta[-1])==0 & abs(true.beta)>0);
      log.lik[subset.index,Rep] <- this.log.lik;
      adjr2[subset.index,Rep] <- summary(this.model)$adj.r.squared;
      nPredIn[subset.index,Rep] <- sum(this.subset);
   } 
}

aic <- -2*log.lik+2*nPredIn;
aic3 <- -2*log.lik+3*nPredIn;
bic <- -2*log.lik+log(n)*nPredIn;
bic.star <- -2*log.lik+log((n+2)/24)*nPredIn;
caic <- -2*log.lik+(log(n)+1)*nPredIn;
aicc <- aic + 2*(nPredIn+1+1)*(nPredIn+2+1)/(n-nPredIn-1-2);
ric <- -2*log.lik+(2*log(nPredAvailable))*nPredIn;

aic.pick <- apply(aic,2,which.min);    
  aic3.pick <- apply(aic3,2,which.min);
  bic.pick <- apply(bic,2,which.min);
  bic.star.pick <- apply(bic.star,2,which.min);
  caic.pick <- apply(caic,2,which.min);
  aicc.pick <- apply(aicc,2,which.min);
  ric.pick <- apply(ric,2,which.min);
  adjr2.pick <- rep(apply(adjr2,2,which.max),nReps);
  full.pick <- rep(1,nReps);
  null.pick <- rep(nrow(subsets),nReps);
  for (subsetrow in 1:nrow(subsets)) {
    if (mean(subsets[subsetrow,]==true.subset)==1) {
      truth.pick <- rep(subsetrow,nReps);
    }
  }
aic.mse <- matrix(NA,nReps,5);
  aic3.mse <- matrix(NA,nReps,5);
  bic.mse <- matrix(NA,nReps,5);
  bic.star.mse <- matrix(NA,nReps,5);
  caic.mse <- matrix(NA,nReps,5);
  aicc.mse <- matrix(NA,nReps,5);
  ric.mse <- matrix(NA,nReps,5);
  adjr2.mse <- matrix(NA,nReps,5);
  full.mse <- matrix(NA,nReps,5);
  null.mse <- matrix(NA,nReps,5);
  truth.mse <- matrix(NA,nReps,5);
aic.size <- rep(NA,nReps);
  aic3.size <- rep(NA,nReps);
  bic.size <- rep(NA,nReps);
  bic.star.size <- rep(NA,nReps);
  caic.size <- rep(NA,nReps);
  aicc.size <- rep(NA,nReps);
  ric.size <- rep(NA,nReps);
  adjr2.size <- rep(NA,nReps);
  full.size <- rep(NA,nReps);
  null.size <- rep(NA,nReps);
  truth.size <- rep(NA,nReps);
aic.fn <- rep(NA,nReps);
  aic3.fn <- rep(NA,nReps);
  bic.fn <- rep(NA,nReps);
  bic.star.fn <- rep(NA,nReps);
  caic.fn <- rep(NA,nReps);
  aicc.fn <- rep(NA,nReps);
  ric.fn <- rep(NA,nReps);
  adjr2.fn <- rep(NA,nReps);
  full.fn <- rep(NA,nReps);
  null.fn <- rep(NA,nReps);
  truth.fn <- rep(NA,nReps);
aic.fp <- rep(NA,nReps);
  aic3.fp <- rep(NA,nReps);
  bic.fp <- rep(NA,nReps);
  bic.star.fp <- rep(NA,nReps);
  caic.fp <- rep(NA,nReps);
  aicc.fp <- rep(NA,nReps);
  ric.fp <- rep(NA,nReps);
  adjr2.fp <- rep(NA,nReps);
  full.fp <- rep(NA,nReps);
  null.fp <- rep(NA,nReps);
  truth.fp <- rep(NA,nReps);

for (Rep in 1:nReps) {
    aic.size[Rep] <- sum(subsets[aic.pick[Rep],]);
      aic3.size[Rep] <- sum(subsets[aic3.pick[Rep],]);
      bic.size[Rep] <- sum(subsets[bic.pick[Rep],]);
      bic.star.size[Rep] <- sum(subsets[bic.star.pick[Rep],]);
      caic.size[Rep] <- sum(subsets[caic.pick[Rep],]);
      aicc.size[Rep] <- sum(subsets[aicc.pick[Rep],]);
      ric.size[Rep] <- sum(subsets[ric.pick[Rep],]);
      adjr2.size[Rep] <- sum(subsets[adjr2.pick[Rep],]);
      full.size[Rep] <- sum(subsets[full.pick[Rep],]);
      null.size[Rep] <- sum(subsets[null.pick[Rep],]);
      truth.size[Rep] <- sum(subsets[truth.pick[Rep],]);
    aic.fp[Rep] <- sum(subsets[aic.pick[Rep],]& !true.subset);
      aic3.fp[Rep] <- sum(subsets[aic3.pick[Rep],]& !true.subset);
      bic.fp[Rep] <- sum(subsets[bic.pick[Rep],]& !true.subset);
      bic.star.fp[Rep] <- sum(subsets[bic.star.pick[Rep],]& !true.subset);
      caic.fp[Rep] <- sum(subsets[caic.pick[Rep],]& !true.subset);
      aicc.fp[Rep] <- sum(subsets[aicc.pick[Rep],]& !true.subset);
      ric.fp[Rep] <- sum(subsets[ric.pick[Rep],]& !true.subset);
      adjr2.fp[Rep] <- sum(subsets[adjr2.pick[Rep],]& !true.subset);
      full.fp[Rep] <- sum(subsets[full.pick[Rep],]& !true.subset);
      null.fp[Rep] <- sum(subsets[null.pick[Rep],]& !true.subset);
      truth.fp[Rep] <- sum(subsets[truth.pick[Rep],]& !true.subset);
    aic.fn[Rep] <- sum(!subsets[aic.pick[Rep],]& true.subset);
      aic3.fn[Rep] <- sum(!subsets[aic3.pick[Rep],]& true.subset);
      bic.fn[Rep] <- sum(!subsets[bic.pick[Rep],]& true.subset);
      bic.star.fn[Rep] <- sum(!subsets[bic.star.pick[Rep],]& true.subset);
      caic.fn[Rep] <- sum(!subsets[caic.pick[Rep],]& true.subset);
      aicc.fn[Rep] <- sum(!subsets[aicc.pick[Rep],]& true.subset);
      ric.fn[Rep] <- sum(!subsets[ric.pick[Rep],]& true.subset);
      adjr2.fn[Rep] <- sum(!subsets[adjr2.pick[Rep],]& true.subset);
      full.fn[Rep] <- sum(!subsets[full.pick[Rep],]& true.subset);
      null.fn[Rep] <- sum(!subsets[null.pick[Rep],]& true.subset);
      truth.fn[Rep] <- sum(!subsets[truth.pick[Rep],]& true.subset);
    for (kind in 1:5) {
      aic.mse[Rep,kind] <- mse[aic.pick[Rep],Rep,kind];
      aic3.mse[Rep,kind] <- mse[aic3.pick[Rep],Rep,kind];
      bic.mse[Rep,kind] <- mse[bic.pick[Rep],Rep,kind];
      bic.star.mse[Rep,kind] <- mse[bic.star.pick[Rep],Rep,kind];
      caic.mse[Rep,kind] <- mse[caic.pick[Rep],Rep,kind];
      aicc.mse[Rep,kind] <- mse[aicc.pick[Rep],Rep,kind];
      ric.mse[Rep,kind] <- mse[ric.pick[Rep],Rep,kind];
      adjr2.mse[Rep,kind] <- mse[adjr2.pick[Rep],Rep,kind];
      full.mse[Rep,kind] <- mse[full.pick[Rep],Rep,kind];
      null.mse[Rep,kind] <- mse[null.pick[Rep],Rep,kind];
      truth.mse[Rep,kind] <- mse[truth.pick[Rep],Rep,kind];
   }
}

Rep.is.bad <- apply(is.nan(mse)&is.na(mse),2,sum)>0;
results <- c(   true.subset.size = sum(true.subset),
                proportion.bad = mean(Rep.is.bad),
                aic.mse1 = mean(aic.mse[which(!Rep.is.bad),1]),
                  aic3.mse1 = mean(aic3.mse[which(!Rep.is.bad),1]),
                  bic.mse1 = mean(bic.mse[which(!Rep.is.bad),1]),
                  bic.star.mse1 = mean(bic.star.mse[which(!Rep.is.bad),1]),
                  caic.mse1 = mean(caic.mse[which(!Rep.is.bad),1]),
                  aicc.mse1 = mean(aicc.mse[which(!Rep.is.bad),1]),
                  ric.mse1 = mean(ric.mse[which(!Rep.is.bad),1]),
                  adjr2.mse1 = mean(adjr2.mse[which(!Rep.is.bad),1]),
                  full.mse1 = mean(full.mse[which(!Rep.is.bad),1]),
                  null.mse1 = mean(null.mse[which(!Rep.is.bad),1]),
                  truth.mse1 = mean(truth.mse[which(!Rep.is.bad),1]),
                aic.mse2 = mean(aic.mse[which(!Rep.is.bad),2]),
                  aic3.mse2 = mean(aic3.mse[which(!Rep.is.bad),2]),
                  bic.mse2 = mean(bic.mse[which(!Rep.is.bad),2]),
                  bic.star.mse2 = mean(bic.star.mse[which(!Rep.is.bad),2]),
                  caic.mse2 = mean(caic.mse[which(!Rep.is.bad),2]),
                  aicc.mse2 = mean(aicc.mse[which(!Rep.is.bad),2]),
                  ric.mse2 = mean(ric.mse[which(!Rep.is.bad),2]),
                  adjr2.mse2 = mean(adjr2.mse[which(!Rep.is.bad),2]),
                  full.mse2 = mean(full.mse[which(!Rep.is.bad),2]),
                  null.mse2 = mean(null.mse[which(!Rep.is.bad),2]),
                  truth.mse2 = mean(truth.mse[which(!Rep.is.bad),2]),
                aic.mse3 = mean(aic.mse[which(!Rep.is.bad),3]),
                  aic3.mse3 = mean(aic3.mse[which(!Rep.is.bad),3]),
                  bic.mse3 = mean(bic.mse[which(!Rep.is.bad),3]),
                  bic.star.mse3 = mean(bic.star.mse[which(!Rep.is.bad),3]),
                  caic.mse3 = mean(caic.mse[which(!Rep.is.bad),3]),
                  aicc.mse3 = mean(aicc.mse[which(!Rep.is.bad),3]),
                  ric.mse3 = mean(ric.mse[which(!Rep.is.bad),3]),
                  adjr2.mse3 = mean(adjr2.mse[which(!Rep.is.bad),3]),
                  full.mse3 = mean(full.mse[which(!Rep.is.bad),3]),
                  null.mse3 = mean(null.mse[which(!Rep.is.bad),3]),
                  truth.mse3 = mean(truth.mse[which(!Rep.is.bad),3]),
                aic.mse4 = mean(aic.mse[which(!Rep.is.bad),4]),
                  aic3.mse4 = mean(aic3.mse[which(!Rep.is.bad),4]),
                  bic.mse4 = mean(bic.mse[which(!Rep.is.bad),4]),
                  bic.star.mse4 = mean(bic.star.mse[which(!Rep.is.bad),4]),
                  caic.mse4 = mean(caic.mse[which(!Rep.is.bad),4]),
                  aicc.mse4 = mean(aicc.mse[which(!Rep.is.bad),4]),
                  ric.mse4 = mean(ric.mse[which(!Rep.is.bad),4]),
                  adjr2.mse4 = mean(adjr2.mse[which(!Rep.is.bad),4]),
                  full.mse4 = mean(full.mse[which(!Rep.is.bad),4]),
                  null.mse4 = mean(null.mse[which(!Rep.is.bad),4]),
                  truth.mse4 = mean(truth.mse[which(!Rep.is.bad),4]),
                aic.mse5 = mean(aic.mse[which(!Rep.is.bad),5]),
                  aic3.mse5 = mean(aic3.mse[which(!Rep.is.bad),5]),
                  bic.mse5 = mean(bic.mse[which(!Rep.is.bad),5]),
                  bic.star.mse5 = mean(bic.star.mse[which(!Rep.is.bad),5]),
                  caic.mse5 = mean(caic.mse[which(!Rep.is.bad),5]),
                  aicc.mse5 = mean(aicc.mse[which(!Rep.is.bad),5]),
                  ric.mse5 = mean(ric.mse[which(!Rep.is.bad),5]),
                  adjr2.mse5 = mean(adjr2.mse[which(!Rep.is.bad),5]),
                  full.mse5 = mean(full.mse[which(!Rep.is.bad),5]),
                  null.mse5 = mean(null.mse[which(!Rep.is.bad),5]),
                  truth.mse5 = mean(truth.mse[which(!Rep.is.bad),5]),
                aic.size = mean(aic.size[which(!Rep.is.bad)]),
                  aic3.size = mean(aic3.size[which(!Rep.is.bad)]),
                  bic.size = mean(bic.size[which(!Rep.is.bad)]),
                  bic.star.size = mean(bic.star.size[which(!Rep.is.bad)]),
                  caic.size = mean(caic.size[which(!Rep.is.bad)]),
                  aicc.size = mean(aicc.size[which(!Rep.is.bad)]),
                  ric.size = mean(ric.size[which(!Rep.is.bad)]),
                  adjr2.size = mean(adjr2.size[which(!Rep.is.bad)]),
                  full.size = mean(full.size[which(!Rep.is.bad)]),
                  null.size = mean(null.size[which(!Rep.is.bad)]),
                  truth.size = mean(truth.size[which(!Rep.is.bad)]),
                aic.fp = mean(aic.fp[which(!Rep.is.bad)]),
                  aic3.fp = mean(aic3.fp[which(!Rep.is.bad)]),
                  bic.fp = mean(bic.fp[which(!Rep.is.bad)]),
                  bic.star.fp = mean(bic.star.fp[which(!Rep.is.bad)]),
                  caic.fp = mean(caic.fp[which(!Rep.is.bad)]),
                  aicc.fp = mean(aicc.fp[which(!Rep.is.bad)]),
                  ric.fp = mean(ric.fp[which(!Rep.is.bad)]),
                  adjr2.fp = mean(adjr2.fp[which(!Rep.is.bad)]),
                  full.fp = mean(full.fp[which(!Rep.is.bad)]),
                  null.fp = mean(null.fp[which(!Rep.is.bad)]),
                  truth.fp = mean(truth.fp[which(!Rep.is.bad)]),
                aic.fn = mean(aic.fn[which(!Rep.is.bad)]),
                  aic3.fn = mean(aic3.fn[which(!Rep.is.bad)]),
                  bic.fn = mean(bic.fn[which(!Rep.is.bad)]),
                  bic.star.fn = mean(bic.star.fn[which(!Rep.is.bad)]),
                  caic.fn = mean(caic.fn[which(!Rep.is.bad)]),
                  aicc.fn = mean(aicc.fn[which(!Rep.is.bad)]),
                  ric.fn = mean(ric.fn[which(!Rep.is.bad)]),
                  adjr2.fn = mean(adjr2.fn[which(!Rep.is.bad)]),
                  full.fn = mean(full.fn[which(!Rep.is.bad)]),
                  null.fn = mean(null.fn[which(!Rep.is.bad)]),
                  truth.fn = mean(truth.fn[which(!Rep.is.bad)])    );
results.by.subset <- cbind(mse1.na.rm = apply(mse[,,1],1,mean,na.rm=TRUE),
                           mse2.na.rm = apply(mse[,,2],1,mean,na.rm=TRUE),
                           mse3.na.rm = apply(mse[,,3],1,mean,na.rm=TRUE),
                           mse4.na.rm = apply(mse[,,4],1,mean,na.rm=TRUE),
                           mse5.na.rm = apply(mse[,,5],1,mean,na.rm=TRUE),
                           tp.na.rm = apply(true.pos,1,mean,na.rm=TRUE),
                           fp.na.rm = apply(false.pos,1,mean,na.rm=TRUE),
                           tn.na.rm = apply(true.neg,1,mean,na.rm=TRUE),
                           fn.na.rm = apply(false.neg,1,mean,na.rm=TRUE),
                           ll.na.rm = apply(log.lik,1,mean,na.rm=TRUE),
                           np.na.rm = apply(nPredIn,1,mean,na.rm=TRUE),
                           mse1 = apply(mse[,,1],1,mean,na.rm=FALSE),
                           mse2 = apply(mse[,,2],1,mean,na.rm=FALSE),
                           mse3 = apply(mse[,,3],1,mean,na.rm=FALSE),
                           mse4 = apply(mse[,,4],1,mean,na.rm=FALSE),
                           mse5 = apply(mse[,,5],1,mean,na.rm=FALSE),
                           tp = apply(true.pos,1,mean,na.rm=FALSE),
                           fp = apply(false.pos,1,mean,na.rm=FALSE),
                           tn = apply(true.neg,1,mean,na.rm=FALSE),
                           fn = apply(false.neg,1,mean,na.rm=FALSE),
                           ll = apply(log.lik,1,mean,na.rm=FALSE),
                           np = apply(nPredIn,1,mean,na.rm=FALSE) );
time2 <- Sys.time();
rm(adjr2, adjr2.fn , adjr2.fp, adjr2.mse, adjr2.size, aic,
  aic.fn, aic.fp, aic.mse, aic.size, aic3, aic3.fn, aic3.fp,
  aic3.mse, aic3.size, aicc, aicc.fn, aicc.fp, aicc.mse, aicc.size,
  betas, bic, bic.fn, bic.fp, bic.mse, bic.size, bic.star,
  bic.star.fn, bic.star.fp, bic.star.mse, bic.star.size, big.mu,
  big.mu.hat, big.u, caic, caic.fn, caic.fp, caic.mse, caic.size,
  false.neg, false.pos, full.fn, full.fp, full.mse, full.size,
  kind, model1, model2, mse, mu, mu1, mu1star, mu2, mu2star,
  null.fn, null.fp, null.mse, null.size, ric,
  ric.fn, ric.fp, ric.mse, ric.size, subset.index, subsetrow,
  subsets, this.beta, this.log.lik, this.subset, this.x,
  true.neg, true.pos, truth.fn, truth.fp, truth.mse,
  truth.size, u);
save.image(paste("NormalSim",n,"-",which.true.model,".rdata",sep=""));




