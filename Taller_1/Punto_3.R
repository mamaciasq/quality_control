library(qcc)

set.seed(1234)

n=22
X<-matrix(0,nrow=n,ncol=4)
miu=3
sigma=1
deltha=2  

######Se rellena una matriz de 24 filas y 4 columnas

for(i in 1:n){
  if(i<21){
    X[i,]<-rnorm(4,miu,sigma)
  }
  if(i>20){
    X[i,]<-rnorm(4,miu+(deltha*sigma),sigma)
  }  
}

###Grafica de carta x barra

qcc(X,"xbar")
medias=rowMeans(X)
tiempo=seq(0,22-1,by=1)
Xbar=NULL 
for(i in length(tiempo):1){
  Xbar[i]=1/(22-tiempo[i])*sum(medias[length(medias):i])
}

MatrizC=matrix(0,nrow=1,ncol=21)
for(i in 1:21){
  
  MatrizC[i]<-(22-i)*((Xbar[i]-3)^2)
}

MatrizC

#######El punto donde fall? el proceso es el en punto 22, pero se pudo detectar hasta el punto 24

# Desarrolo José 

set.seed(11)
duc <- function(mu0=3,sigma0=1,delta=1,n=4,tau=20)
{
  mu0 <- mu0
  sigma0 <- sigma0
  delta <- delta
  mu1 <- mu0 + (delta*sigma0)
  n <- n
  tau <- tau
  
  LCS <- mu0 + ( (3*sigma0)/sqrt(n) )
  LCI <- mu0 - ( (3*sigma0)/sqrt(n) )
  
  data.control <- matrix(nrow=tau,ncol=n)
  for(i in 1:tau)
  {
    data.control[i,] <- rnorm(n=n,mean=mu0,sd=sigma0)
  }
  info <- list(LCI=LCI,LCS=LCS,data=data.control,mu1=mu1)
  info
}

sigma0 <- 1
delta <- c(1,1.5,2)

limites <- c(duc(mu0=3,sigma=1,delta=1,n=4,tau=20)$LCI,
             duc(mu0=3,sigma=1,delta=1,n=4,tau=20)$LCS)
LCI <- limites[1]
LCS <- limites[2]
data.control <- duc(mu0=3,sigma=1,delta=1,n=4,tau=20)$data

mu1 <- duc(mu0=3,sigma=1,delta=delta,n=4,tau=20)$mu1
data.out.control.delta1 <- list()
aux <- 0
k <- 1

while(aux==0)
{
  data.out.control.delta1[[k]] <- rnorm(n=4,mean=mu1[1],sd=sigma0)
  dv.delta1 <- unlist(data.out.control.delta1[[k]])
  mean.dv.delta1 <- mean(dv.delta1)
  if(mean.dv.delta1>LCS || mean.dv.delta1<LCI)
  {
    aux <- 1
  }
  k <- k+ 1
}
ldocdelta1 <- k- 1

data.out.control.delta15 <- list()
aux <- 0
k <- 1

while(aux==0)
{
  data.out.control.delta15[[k]] <- rnorm(n=4,mean=mu1[2],sd=sigma0)
  dv.delta15 <- unlist(data.out.control.delta15[[k]])
  mean.dv.delta15 <- mean(dv.delta15)
  if(mean.dv.delta15>LCS || mean.dv.delta15<LCI)
  {
    aux <- 1
  }
  k <- k+ 1
}
ldocdelta2 <- k- 1

data.out.control.delta2 <- list()
aux <- 0
k <- 1

while(aux==0)
{
  data.out.control.delta2[[k]] <- rnorm(n=4,mean=mu1[3],sd=sigma0)
  dv.delta2 <- unlist(data.out.control.delta2[[k]])
  mean.dv.delta2 <- mean(dv.delta2)
  if(mean.dv.delta2>LCS || mean.dv.delta2<LCI)
  {
    aux <- 1
  }
  k <- k+ 1
}
ldocdelta3 <- k- 1

dataset.delta1 <- rbind(data.control,
                        matrix(unlist(data.out.control.delta1),nrow=ldocdelta1,ncol=4,byrow=T))
dataset.delta15 <- rbind(data.control,
                         matrix(unlist(data.out.control.delta15),nrow=ldocdelta2,ncol=4,byrow=T))
dataset.delta2 <- rbind(data.control,
                        matrix(unlist(data.out.control.delta2),nrow=ldocdelta3,ncol=4,byrow=T))