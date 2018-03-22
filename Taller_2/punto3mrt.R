# library(qcc)

## Carta CRL

p0<-0.03
ANOS<-50000

L<-log(1-(1/(p0*ANOS)))/log(1-p0)

### Carta Bernoulli CUSUM

# GammaB

p1<-0.0223634
r1<--log((1-p1)/(1-p0))
r2<-log((p1*(1-p0))/(p0*(1-p1)))
gammaB<-r1/r2
gammaB*1000

# hB*

hB<-203.315459
(exp(hB*r2)-(hB*r2)-1)/abs(r2*p0 - r1)

# E(p0)

Eps<-function(x){
  0.410-0.0842*log(x)-0.0391*(log(x))^3-0.00376*(log(x))^4-0.000008*(log(x))^7}
Eps(p0)

# hB

HB<-hB+(Eps(p0)*sqrt(p0*(1-p0)))

### Simulación incluyendo cambios en p

P<-c(0.03,0.05,0.07,0.09,0.1,0.3,0.5,0.7,0.9)

#Evaluando zeta de p

X<-seq(-1000,-0.5,0.001)

Zeta<-function(x,p){
  ((p1/p0)^x)*p+(((1-p1)/(1-p0))^x)*(1-p)
}

ANOScbcs<-c()
ANOScrl<-c()

for(j in 1:length(P)){
  Z<-Zeta(X,P[j])
  zeta<-X[which.min(abs(Z-1))]
  
  ANOSes<-(1/P[j])*(1/(1-(1-P[j])^L))
  ANOScrl<-c(ANOScrl,ANOSes)
  
  ANOScs<-(exp(zeta*hB*r2)-(zeta*hB*r2)-1)/(abs(zeta*((r2*P[j])-r1)))
  ANOScbcs<-c(ANOScbcs,ANOScs)
}

par(bg = "white")
plot(x = P,y = ANOScbcs,pch=15,type="b",col="darkgreen",ylim = c(0,max(ANOScrl)+100),
     main = "ANOS CRL vs. ANOS Bernoulli CUSUM",ylab = "ANOS",xlab = "Probabilidad (p>p_0)")
lines(x = P,y = ANOScrl,pch=17,type="b",col="darkred")
legend("topright", inset=.005,box.lwd = NA,
       c("CRL","Bernoulli CUSUM"), fill=c("darkred","darkgreen"))

#### Gráficas

### Simulación bajo control
set.seed(12345)
Inspeccion<-rbinom(n = 1000,size = 1,prob = 0.02)
CRL <- diff(which(c(1,Inspeccion)==1))-1

### Cálculo de estadísticas de carta Bernoulli CUSUM

B<-c()
B[1]<-0

for(i in 2:1001){
  B[i]<-max(0,B[i-1])+(Inspeccion[i]-gammaB)
}

qcc(CRL, type = "g", center = 1/0.02,limits = c(L,max(CRL)+10))

par(bg = "white")
plot(B[2:30],ylim = c(0,11),main = "Bernoulli CUSUM",ylab = "Estadística Bk",xlab = "Elemento")
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4],col="white")
points(B[2:30],pch=16,type = "b")
abline(h=HB,lty=3)