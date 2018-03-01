n <- 20 # Cantidad de muestras de aprendizaje
m <- c(5,10,20) # Tamaños de muestra
d1 <- c(2.326,3.078,3.735) # Tabla 3.1: Peihua Qiu, Introduction to Statistical Process Control - CRC (2013)
d2 <- c(0.864,0.797,0.729) # Tabla 3.1: Peihua Qiu, Introduction to Statistical Process Control - CRC (2013)
d3 <- gamma(m/2)*sqrt(2/(m-1))/gamma((m-1)/2)
tabla_m <- data.frame(m,d1,d2,d3) # Cantidades d1, d2, d3 asociadas a cada m
a <- c(0.0027,0.001,0.005) # Valores de alpha
k <- c(1.2,1.5,2,3) # valores de delta
mu <- 5 # media de los datos
sigma_0 <- 2 # varainza de la muestra de aprendizaje
sigma_1 <- k*sigma_0 # varianza de la muestra fuera de control
casos <- expand.grid(m,a,sigma_1) 
colnames(casos) <- c("m","a","sigma_1")
library(dplyr)
casos <- casos %>% inner_join(tabla_m)
casos <- casos %>% mutate(ARL_1R=0,ARL_1s=0) # Tabla con todos los casos 
n_c <- nrow(casos) # cantidad de casos
v1 <- 100 # cantidad de veces que se repite la extracción de muestras para crear los límites (el promedio de los ARL_1 sacados en cada una de estas veces nos da una mejor estimación del ARL_1 sin tener en cuenta qué muestra se obtuvo)
t0 <- 200 # cantidad de veces en que, dado que se tienen los límites de confianza ya construidos, se extrae el número de muestras necesarias antes de que la carta detecte el cambio (el promedio de esas tamaños de muestra es el ARL_1 estimado sobre esa muestra)


for(l in 1:n_c){
  
  ARL_1s <- 0
  ARL_1R <- 0
  
  for(v in 1:v1){
    s <- NULL
    R <- NULL
    
    for(i in 1:n){
      x <- rnorm(casos$m[l],mu,sigma_0)
      s[i] <- sd(x)
      R[i] <- abs(range(x)[2]-range(x)[1])
    }
    
    L_s <- max(0,mean(s)*(1-qnorm(1-casos$a[l]/2)*sqrt(1-casos$d3[l]^2)/casos$d3[l]))
    c_s <- mean(s)
    U_s <- mean(s)*(1+qnorm(1-casos$a[l]/2)*sqrt(1-casos$d3[l]^2)/casos$d3[l])
    
    L_R <- max(0,mean(R)*(1-qnorm(1-casos$a[l]/2)*casos$d2[l]/casos$d1[l]))
    c_R <- mean(R)
    U_R <- mean(R)*(1+qnorm(1-casos$a[l]/2)*casos$d2[l]/casos$d1[l])
    
    ARL_1s0 <- 0
    ARL_1R0 <- 0
    
    for(t in 1:t0){
      j <- 0
      c_s0 <- c_s
      while(L_s < c_s0 & U_s > c_s0){
        j <- j + 1
        x <- rnorm(casos$m[l],mu,casos$sigma_1[l])
        c_s0 <- sd(x)
      }
      g <- 0
      c_R0 <- c_R
      while(L_R < c_R0 & U_R > c_R0){
        g <- g + 1
        x <- rnorm(casos$m[l],mu,casos$sigma_1[l])
        c_R0 <- abs(range(x)[2]-range(x)[1])
      }
      ARL_1s0 <- ARL_1s0 + j
      ARL_1R0 <- ARL_1R0 + g
    }
    
    ARL_1s0 <- ARL_1s0/t0
    ARL_1R0 <- ARL_1R0/t0
    
    ARL_1s <- ARL_1s + ARL_1s0
    ARL_1R <- ARL_1R + ARL_1R0
    
    cat("Progreso interno del caso ", l,": ", v*100/v1, "%\n")
    
  }
  
  ARL_1R <- ARL_1R/v1
  ARL_1s <- ARL_1s/v1
  
  casos$ARL_1R[l] <- ARL_1R
  casos$ARL_1s[l] <- ARL_1s
  
  cat("Caso ", l, " completo.\n")
}

library(xtable)
xtable(casos)

write.table(casos, "Resultados.txt", row.names = FALSE, col.names = TRUE)