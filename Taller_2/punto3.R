getwd()
source("/Users/martin/Github/quality_control/Taller_2/funciones_p4.R")
# Par?metros del proceso bajo control
mu0 <- 3
sigma0 <- 1

# N?mero de veces que se calcula el RL (m)
m <- 100
# Tama?o de los subgrupos
n <- 10000

# Valores de delta para monitorear los cambios en la media de mu0 a mu1= mu0 + (delta*sigma0)
delta <- c(0,0.5,1,1.5,2,3)
mu1 <- c()
for(i in 1:length(delta))
{
  mu1[i] <- mu0+ (delta[i]*sigma0)
}

lcxb(mu0=mu0,sigma0=sigma0,n=n)
# 1.5 4.5

cxbd0 <- ARL1cxb(m=m,n=n,mu1=mu1[1],sigma0=sigma0,LCIxb=2.97,LCSxb=3.03)
cxbd05 <- ARL1cxb(m=m,n=n,mu1=mu1[2],sigma0=sigma0,LCIxb=2.97,LCSxb=3.03)
cxbd1 <- ARL1cxb(m=m,n=n,mu1=mu1[3],sigma0=sigma0,LCIxb=2.97,LCSxb=3.03)
cxbd15 <- ARL1cxb(m=m,n=n,mu1=mu1[4],sigma0=sigma0,LCIxb=2.97,LCSxb=3.03)
cxbd2 <- ARL1cxb(m=m,n=n,mu1=mu1[5],sigma0=sigma0,LCIxb=2.97,LCSxb=3.03)
cxbd3 <- ARL1cxb(m=m,n=n,mu1=mu1[6],sigma0=sigma0,LCIxb=2.97,LCSxb=3.03)

