# Punto 1

p <- c(0.6827,0,0)
R <- matrix(c(0.6827,0,0,0,0,0,0,0,0),nrow=3,byrow=T)

id <- matrix(c(1,0,0,0,1,0,0,0,1),nrow=3,byrow=T)

t(p) %*% solve(id-R) %*% c(1,1,1)

