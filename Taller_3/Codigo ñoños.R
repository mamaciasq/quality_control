library(ggplot2)

Data<-read.csv("CHEMICAL_PROCESS.csv",header = T,sep = ";",dec = ",")
Data<-Data[,2:5]

### Caracterización, por medio de la distancia de mahalanobis

# se realiza una caracterización inicial con la distancia de Mahalanobis

M<-as.vector(apply(Data,MARGIN = 2,mean))
S<-var(Data)

D1<-mahalanobis(Data,center = M,cov = S)
D1<-data.frame(N=1:20,D1)

ggplot(D1,aes(x=N,y = D1,label=N))+
  geom_point(color="darkblue",size=2)+
  theme_bw()+labs(title="",x="Observacion",y="Dm",caption="Chemical Process Data")+
  geom_text(hjust = 0.05, nudge_x = 0.5)

### Determinación de atípicos por el estimador de volumen mínimo

library(MASS)
set.seed(12345)
MVE<-cov.rob(x = Data,cor = TRUE,method = "mve",nsamp = "exact")

### Identificación de datos atípicos

ggplot(D1,aes(x=N,y = D1,label=N))+
  geom_point(color=ifelse(D %in% D[MVE$best],"darkblue","darkorange"),size=2)+
  theme_bw()+labs(title="",x="Observación",y="Dm",caption="Chemical Process Data")+
  geom_text(hjust = 0.05, nudge_x = 0.5)


D2<-mahalanobis(Data,center = MVE$center,cov = MVE$cov)
D2<-data.frame(N=1:20,D2)

ggplot(D2,aes(x=N,y = D2,label=N))+
  geom_point(color=ifelse(D %in% D[MVE$best],"darkblue","darkorange"),size=2)+
  theme_bw()+labs(title="",x="Observación",y="Dm",caption="Chemical Process Data")+
  geom_text(hjust = 0.05, nudge_x = 0.5)

##### Fase I - Etapa 1

Data2<-Data[MVE$best,]
Data2$T2<-c(NA)

for(i in 1:dim(Data2)[1]){
  Data2$T2[i]<-t(t(as.vector(Data2[i,1:4])-as.vector(MVE$center)))%*%
    solve(MVE$cov)%*%t(as.vector(Data2[i,1:4])-as.vector(MVE$center))  
}

par(bg = "gray88")
plot(Data2$T2,ylim = c(0,10),main = "Carta de control Fase I - Etapa I",ylab = "T2",xlab = "Observación")
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4],col="white")
points(Data2$T2,pch=16,type = "b")
abline(h=((11^2)/12)*qbeta((1-0.0005)^(1/12),shape1 = 2,shape2 = 3.5),lty=3)

##### Fase II

NData<-read.csv("NEW_DATA_CHEMICAL_PROCESS.csv",header = T,sep = ";",dec = ",")
NData<-NData[,2:5]

NData$T2<-c(NA)

for(i in 1:dim(NData)[1]){
  NData$T2[i]<-t(t(as.vector(NData[i,1:4])-as.vector(MVE$center)))%*%
    solve(MVE$cov)%*%t(as.vector(NData[i,1:4])-as.vector(MVE$center))  
}

h<-qchisq((1-0.0005),4)
par(bg = "gray88")
plot(NData$T2,ylim = c(0,600),main = expression(paste("Carta de control ",chi^2)),ylab = "T2",xlab = "Observación")
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4],col="white")
points(NData$T2,pch=16,type = "b",col=ifelse(NData$T2 > h,'red','black'))
abline(h=h,lty=3)