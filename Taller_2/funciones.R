############################################################################################
# CÁLCULO DE LOS LÍMITES DE CONTROL EN LAS CARTAS X BARRA, CUSUM Y EWMA PARA UN ARL0
lcxb <- function(mu0=3,sigma0=1,n=4)
{
  LCIxb <- mu0- (( 3*sigma0 )/sqrt(n))
  LCSxb <- mu0+ (( 3*sigma0 )/sqrt(n))
  limites <- c(LCIxb,LCSxb)
  limites
}

lcCUSUM <- function(h=3.34)
{
  LCICUSUM <- (-1)*h
  LCSCUSUM <- h
  limites <- c(LCICUSUM,LCSCUSUM)
  limites
}

lcEWMA <- function(mu0=3,L=2.75,lambda=0.12,sigma0=1,n=4)
{
  LCIEWMA <- mu0- (L*sqrt( (lambda*sigma0^2)/((2- lambda)*n) ))
  LCSEWMA <- mu0+ (L*sqrt( (lambda*sigma0^2)/((2- lambda)*n) ))
  limites <- c(LCIEWMA,LCSEWMA)
  limites
}
############################################################################################

############################################################################################
# CÁLCULO DEL ARL1 EN LAS CARTAS X BARRA, CUSUM Y EWMA
ARL1cxb <- function(m=15,n=4,mu1=3.01,sigma0=1,LCIxb=1.5,LCSxb=4.5)
{
  RLxb <- c()
  
  for(i in 1:m)
  {
    doutc <- list()
    auxmean <- 0
    amean <- 1
    
    while(auxmean==0)
    {
      doutc[[amean]] <- rnorm(n=n,mean=mu1,sd=sigma0)
      dv <- unlist(doutc[[amean]])
      mean.dv <- mean(dv)
      if(mean.dv>=LCSxb || mean.dv<=LCIxb)
      {
        auxmean <- 1
      }
      amean <- amean+ 1
      rm(dv,mean.dv)
    }
    amean <- amean- 1
    RLxb[i] <- amean
    rm(doutc)
  }
  ARL1xb <- mean(RLxb)
  ARL1xb
}

ARL1cCUSUM <- function(m=15,n=4,mu0=3,mu1=3.01,sigma0=1,k=0.75,LCICUSUM=-3.34,LCSCUSUM=3.34)
{
  RLCUSUM <- c()
  for(i in 1:m)
  {
    SH0 <- 0 -> SL0
    SHn <- list()
    SLn <- list()
    doutc <- list()
    auxmean <- 0
    amean <- 1
    
    while(auxmean==0)
    {
      doutc[[amean]] <- rnorm(n=n,mean=mu1,sd=sigma0)
      dv <- unlist(doutc[[amean]])
      mean.dv <- mean(dv)
      Z <- (mean.dv- mu0)/(sigma0/sqrt(n))
      if(amean==1)
      {
        SHn[[amean]] <- max(0,SH0+ Z- k)
        SLn[[amean]] <- min(0,SL0+ Z+ k)
      }
      if(amean>1)
      {
        SHn[[amean]] <- max(0,SHn[[amean- 1]]+ Z- k)
        SLn[[amean]] <- min(0,SLn[[amean- 1]]+ Z+ k)
      }
      if(SHn[[amean]]>=LCSCUSUM || SLn[[amean]]<=LCICUSUM)
      {
        auxmean <- 1
      }
      amean <- amean+ 1
      rm(dv,mean.dv,Z)
    }
    rm(doutc,SLn,SHn)
    amean <- amean- 1
    RLCUSUM[i] <- amean
  }
  ARL1CUSUM <- mean(RLCUSUM)
  ARL1CUSUM
}

ARL1cEWMA <- function(m=15,n=4,mu1=3.01,sigma0=1,lambda=0.12,LCIEWMA=2.6526,LCSEWMA=3.3473)
{
  RLEWMA <- c()
  for(i in 1:m)
  {
    Z0 <- mu0
    Zt <- list()
    doutc <- list()
    auxmean <- 0
    amean <- 1
    
    while(auxmean==0)
    {
      doutc[[amean]] <- rnorm(n=n,mean=mu1,sd=sigma0)
      dv <- unlist(doutc[[amean]])
      mean.dv <- mean(dv)
      if(amean==1)
      {
        Zt[[amean]] <- (lambda*mean.dv)+ ((1- lambda)*Z0)
      }
      if(amean>1)
      {
        Zt[[amean]] <- (lambda*mean.dv)+ ((1- lambda)*Zt[[amean- 1]])
      }
      if(Zt[[amean]]>=LCSEWMA || Zt[[amean]]<=LCIEWMA)
      {
        auxmean <- 1
      }
      amean <- amean+ 1
      rm(dv,mean.dv)
    }
    amean <- amean- 1
    RLEWMA[i] <- amean
    rm(Zt,doutc)
  }
  ARL1EWMA <- mean(RLEWMA)
  ARL1EWMA
}
############################################################################################