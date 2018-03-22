# Punto 4, taller 2

mu0 <- 3
sigma <- 1
n <- 1000
k <- 3
kprima <- 2
d1 <- 0.1
d2 <- 1.9
m <- 100


i1ii <- mu0 - (k*sigma/sqrt(n))
i1is <- mu0 - (kprima*sigma/sqrt(n))
i1si <- mu0 + (kprima*sigma/sqrt(n))
i1ss <- mu0 + (k*sigma/sqrt(n))
i2i <- mu0 - (kprima*sigma/sqrt(n))
i2s <- mu0 + (kprima*sigma/sqrt(n))



Tt <- c()
lcs <- mu0 + k*sigma/sqrt(n)
lci <- mu0 - k*sigma/sqrt(n)

for (j in 1:m) {
  i <- 0
  r <- 1
  t <- c()
  while (i == 0) {
    data <- rnorm(n=n, mean=mu0, sd = sigma)
    media <- mean(data)
    if (media >= i2i && media <= i2s)
      t[r] = d2
    if ((media >= i1ii && media <= i1is)||(media >= i1si && media <= i1ss))
      t[r] = d1
    if (media >= lcs || media <= lci)
      i = 1
    r=r+1
  }
  Tt[j] = sum(t)
}

ats <- mean(Tt)
