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
