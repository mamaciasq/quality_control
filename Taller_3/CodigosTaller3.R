library(readxl)
library(dplyr)
daticos <- read.delim("~/Downloads/daticos.txt", header=TRUE)
dataf1 <- daticos[1:20,2:5]
dataf2 <- daticos[21:30,2:5]

################################################################################################
##################### OBSERVACIONES INDIVIDUALES
##### FASE I ETAPA I: DISTRIBUCI”N BETA
f1e1 <- function(data,alpha=0.05,graph=1)
{
  data <- data
  ddata <- dim(data)
  m <- ddata[1]
  p <- ddata[2]
  hat.mu <- matrix(nrow=1,ncol=p)
  for(i in 1:p)
  {
    hat.mu[1,i] <- (1/m)*sum(data[,i])
  }
  hat.mu <- t(hat.mu)
  hat.sigma <- ( (m- 1)/m )*var(data)
  T2 <- c()
  for(i in 1:m)
  {
    T2[i] <- t(t(data[i,])- hat.mu) %*% solve(hat.sigma) %*% (t(data[i,])- hat.mu)
  }
  LCS <- ( ((m-1)^2)/m )*qbeta(p=(1-alpha),shape1=(p/2),shape2=((m- p- 1)/2))
  resumen <- list()
  if(graph==1)
  {
    plot(seq(from=1,to=m,by=1),T2,xlab="Observación",ylab="Estadística T2",
         main="Carta T2 para la fase I Etapa I",ylim=c(0,max((LCS+1),max(T2))),
         type="b")
    abline(h=LCS,col="red")
    resumen <- list(LCS=LCS,T2=T2,mu=hat.mu,Sigma=hat.sigma)
    resumen
  }
  if(graph==0)
  {
    resumen <- list(LCS=LCS,T2=T2,mu=hat.mu,Sigma=hat.sigma)
    resumen
  }
  resumen <- list(LCS=LCS,T2=T2,mu=hat.mu,Sigma=hat.sigma)
}
cartaT2f1e1 <- f1e1(data=dataf1,alpha=0.05,graph=1)
mu0 <- cartaT2f1e1$mu
Sigma0 <- cartaT2f1e1$Sigma
cartaT2f1e1$LCS


##### FASE 2: DISTRIBUCI”N CHI-CUADRADO
f2 <- function(data,mu0,Sigma,alpha=0.05,graph=1)
{
  data <- data
  mu0 <- t(t(mu0))
  Sigma <- Sigma
  
  ddata <- dim(data)
  m <- ddata[1]
  p <- ddata[2]
  
  T2 <- c()
  for(i in 1:m)
  {
    T2[i] <- t(t(data[i,])- mu0) %*% solve(Sigma) %*% (t(data[i,])- mu0)
  }
  LCS <- qchisq(p=(1-alpha),df=p)
  resumen <- list()
  if(graph==1)
  {
    plot(seq(from=1,to=m,by=1),T2,xlab="Observación",ylab="Estadística T2",
         main="Carta T2 para la fase II",ylim=c(0,max((LCS+1),max(T2))),
         type="b")
    abline(h=LCS,col="red")
    resumen <- list(LCS=LCS,T2=T2,mu=mu0,Sigma=Sigma)
    resumen
  }
  if(graph==0)
  {
    resumen <- list(LCS=LCS,T2=T2,mu=mu0,Sigma=Sigma)
    resumen
  }
  resumen <- list(LCS=LCS,T2=T2,mu=mu0,Sigma=Sigma)
}
# Los resultados utilizando cualquiera de las dos funciones (la que construimos o la mult.chart)
#  son los mismos. Puede correrlo para verificar esto
cartaT2f2 <- f2(data=dataf2,mu0=mu0,alpha=0.05,Sigma=Sigma0,graph=1)
# Usando la funciÛn mult.chart()
library(MSQC)
mult.chart(type="chi", Xmv = mu0, S=Sigma0, x = dataf2,alpha=0.05, phase = 2)

data(carbon2)
carbon1

################################################################################################

# Con el paquete MSQC
mult.chart(dataf1, type = "t2",alpha = 0.05)
Xmv <- mult.chart(dataf1, type = "t2",alpha = 0.05) $Xmv
S <- mult.chart(dataf1, type = "t2",alpha = 0.05) $covariance
carta <- mult.chart(dataf1, type = "t2",alpha = 0.05)
mult.chart(dataf2,type="chi", Xmv = Xmv, S = S, colm = 20, alpha = 0.05)
mult.chart(type = "mewma", dataf2, Xmv = Xmv, S = S, alpha = 0.05)
mult.chart(type = "mcusum", dataf2, Xmv = Xmv, S = S, alpha = 0.05)
