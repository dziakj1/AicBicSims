rm(list=ls(all=TRUE));
true.number.of.factors <- 3;
max.number.of.factors <- 5;
nrep <- 1000;
sizes <- (5:60)*10;
require(stats)
library(datasets)

wishart.modified <- function(n,S){
    # Function provided by Donna Coffman;
    nu <- n-1;
    nvars <- nrow(S)
    Ch <- chol(S)
    #load diagonal elements with square root of chi square variates
    a <- diag(sqrt(rchisq(nvars,(nu-(0:nvars-1)))))
    #load upper triangle with independent normal (0,1) variates
    a[upper.tri(a)] <- rnorm(nvars*(nvars-1)/2)
    #Desired matrix is
    x <- a%*%Ch
    W <- t(x)%*%x;
    return(W);
}

orig.data.corrmat <- Harman74.cor$cov;
true.models <- list();
for (nFactors in 1:max.number.of.factors) {
   true.models[[nFactors]] <- factanal(factors=nFactors, covmat=Harman74.cor);
   print(true.models[[nFactors]]);
}
this.true.model <- true.models[[true.number.of.factors]];
L <- this.true.model$loadings;
this.true.corrmat <- L%*%t(L) + diag(this.true.model$uniquenesses);

err1 <- array(0, c(length(sizes),nrep, nFactors));
err2 <- array(0, c(length(sizes),nrep, nFactors));
err3 <- array(0, c(length(sizes),nrep, nFactors));
err4 <- array(0, c(length(sizes),nrep, nFactors));
logliks.matrix <- array(0, c(length(sizes),nrep, nFactors));
num.params.matrix <- array(0, c(length(sizes),nrep, nFactors));

for (this.size in 1:length(sizes)) {
   n <- sizes[this.size];
   for (this.rep in 1:nrep) {
      print(this.rep);
      sim.covmat <- wishart.modified(n=n,S=this.true.corrmat)/(n-1);
      sim.corrmat <- sim.covmat%*%diag(1/diag(sim.covmat));
      fitted.models <- list();
      fitted.covmats <- list();
      fitted.corrmats <- list();
      num.params <- list();
      for (nFactors in 1:max.number.of.factors) {
         # based on Akaike 1987
         for (tries in 1:50) {
            temp <- try(factanal(factors=nFactors, covmat=sim.covmat,control=list(nstart=3)));
            if (class(temp)=="factanal") {
               fitted.models[[nFactors]] <- temp;
               break;
            }
         }
         if (class(temp)!="factanal") {stop("The factor analysis could not be done.")};
         L <- fitted.models[[nFactors]]$loadings;
         fitted.covmats[[nFactors]] <- L%*%t(L) + diag(fitted.models[[nFactors]]$uniquenesses);
         fitted.corrmats[[nFactors]] <- fitted.covmats[[nFactors]]%*%diag(1/diag(fitted.covmats[[nFactors]])); # almost the same;
         this.likelihood <-  -.5*n*(log(det(fitted.covmats[[nFactors]]))+sum(diag(solve(fitted.covmats[[nFactors]],sim.covmat))));
         print(this.likelihood);
         p <- nrow(orig.data.corrmat);
         k <- nFactors;
         num.params[[nFactors]] <-  .5*(2*p*(k+1)  -  k*(k-1));  # Akaike 1987 p. 320?
                                      # loadings      factor
                                      # and           covariance
                                      # uniquenesses  matrix
         logliks.matrix[this.size,this.rep,nFactors] <- this.likelihood;
         num.params.matrix[this.size,this.rep,nFactors] <- num.params[[nFactors]];
         err1[this.size,this.rep,nFactors] <- mean((fitted.covmats[[nFactors]]-sim.covmat)^2);
         err2[this.size,this.rep,nFactors] <- mean((fitted.covmats[[nFactors]]-this.true.corrmat)^2);
         err3[this.size,this.rep,nFactors] <- mean((fitted.corrmats[[nFactors]]-sim.covmat)^2);
         err4[this.size,this.rep,nFactors] <- mean((fitted.corrmats[[nFactors]]-this.true.corrmat)^2);
      }
   }
   nstar <- (n + 2) / 24;
   save.image(paste("working-on-",true.number.of.factors,".rdata",sep=""));
}

AICs <-     -2*logliks.matrix + 2*num.params.matrix;
AdjBICs <-  -2*logliks.matrix + (log(nstar))*num.params.matrix;
BICs <-     -2*logliks.matrix + (log(n))*num.params.matrix;
CAICs <-    -2*logliks.matrix + (log(n)+1)*num.params.matrix;

picks.AIC <- apply(AICs,c(1,2),which.min);
picks.AdjBIC <- apply(AdjBICs,c(1,2),which.min);
picks.BIC <- apply(BICs,c(1,2),which.min);
picks.CAIC <- apply(CAICs,c(1,2),which.min);


over.AIC <- picks.AIC > true.number.of.factors;
over.AdjBIC <- picks.AdjBIC > true.number.of.factors;
over.BIC <- picks.BIC > true.number.of.factors;
over.CAIC <- picks.CAIC > true.number.of.factors;
under.AIC <- picks.AIC < true.number.of.factors;
under.AdjBIC <- picks.AdjBIC < true.number.of.factors;
under.BIC <- picks.BIC < true.number.of.factors;
under.CAIC <- picks.CAIC < true.number.of.factors;
at.AIC <- picks.AIC == true.number.of.factors;
at.AdjBIC <- picks.AdjBIC == true.number.of.factors;
at.BIC <- picks.BIC == true.number.of.factors;
at.CAIC <- picks.CAIC == true.number.of.factors;

answers <- matrix(c( apply(over.AIC,1,mean),
                     apply(over.AdjBIC,1,mean),
                     apply(over.BIC,1,mean),
                     apply(over.CAIC,1,mean),
                     apply(under.AIC,1,mean),
                     apply(under.AdjBIC,1,mean),
                     apply(under.BIC,1,mean),
                     apply(under.CAIC,1,mean),
                     apply(at.AIC,1,mean),
                     apply(at.AdjBIC,1,mean),
                     apply(at.BIC,1,mean),
                     apply(at.CAIC,1,mean) ),    length(sizes),12);
print(round(answers,2));

save.image(paste("did-",true.number.of.factors,".rdata",sep=""));
