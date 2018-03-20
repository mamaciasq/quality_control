library(qcc)
library(spc)
set.seed(50)
n<-20 
X<-matrix(0,nrow=n,ncol=1) 
mu=10
sigma=2 
deltha=3 
for(i in 1:n)
  {
  if(i<16)
    { X[i]<-rnorm(1,mu,sigma)
    } 
  if(i>15){
    X[i]<-rnorm(1,mu+(deltha*sigma),sigma) 
    }
  }
(limewma<-xewma.crit(l=0.2,L0=200)) 
ewma(X,lambda=0.2,label.limits=c(-limewma,limewma)) 

limqcc<-xshewhartrunsrules.crit(L0=200,type="1") 
qcc(X,"xbar.one",nsigmas=3*limqcc) 

(limcusum<-xcusum.crit(k=0.4,L0=200)) 
cusum(X,decision.interval=c(-limcusum,limcusum))
