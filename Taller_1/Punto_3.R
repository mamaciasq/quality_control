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
