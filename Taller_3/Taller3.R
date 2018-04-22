library(readxl)
library(dplyr)
data <- read_excel("~/Downloads/tabla_datos_taller3_160418.xlsx")
View(data)
data <- data[1:20,2:5]
data2 <- data[22:31,2:5]
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
    plot(seq(from=1,to=m,by=1),T2,xlab="ObservaciÛn",ylab="EstadÌstica T2",
         main="Carta T2 para la fase I Etapa I",ylim=c(0,(LCS+1)),
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
cartaT2f1e1 <- f1e1(data=data,alpha=0.05,graph=1)
################################################################################################

mu <- cartaT2f1e1$mu
sigma <- cartaT2f1e1$Sigma
ucl = cartaT2f1e1$LCS

library(MSQC)
mult.chart(type="chi", Xmv = mu, S=sigma, x = data2, phase = 2, ucl = ucl, alpha = 0.01)
