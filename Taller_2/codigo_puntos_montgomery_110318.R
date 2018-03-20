#################################################################
##### Punto 6.4.
# Parte a. Realizar una carta para monitorear la proporci?n
#  no conforme (carta p)
library(readxl)
library(qcc)
# Datos de la pesta?a "d6.4" de Excel
dp64 <- read_excel("Taller_2/dtc_taller2_060318.xlsx", sheet = "d6.4")
#dp64 <- read.delim("clipboard")
cpdp64 <- qcc(data=dp64[,2],type="p",sizes=150,plot=T,
              title="Carta p para monitorear la proporción
              no conforme")
# Con base en la carta se puede afirmar que el proceso se 
#  encuentra bajo control
# Parte b. Calcular un tama?o de muestra para...
#################################################################

#################################################################
##### Punto 6.5.
# Parte a. Realizar una carta para monitorear la proporci?n
#  no conforme (carta p)
# Datos de la pesta?a "d6.5" de Excel
dp65 <- read_excel("Taller_2/dtc_taller2_060318.xlsx", sheet = "d6.5")
cpdp65 <- qcc(data=dp65[,2],type="p",sizes=2500,plot=T,
              title="Carta p para monitorear la proporci?n
              no conforme")
# Seg?n los c?lculos, los l?mites de control ser?an
#  LCI= 0.1031262 y LCS= 0.1425138. Sin embargo, en la
#  carta se puede apreciar que hay muchas observaciones
#  por fuera de los l?mites de control. Por lo cual,
#  lo natural ser?a eliminar estas observaciones para determinar
#  cu?les ser?an los l?mites de un proceso bajo control 
#  (parte b del punto).
cpdp65 <- qcc(data=dp65[-c(1,2,3,5,11,12,15,16,17,19,20),2],
              type="p",sizes=2500,plot=T,
              title="Carta p para monitorear la proporci?n
              no conforme")
# En este caso, los l?mites de control ser?an
#  LCI= 0.1048048 y LCS= 0.1444396
#################################################################

#################################################################
##### Punto 8.4.
# Parte a. Realizar una carta CUSUM para monitorear la 
#  media del proceso. En este caso, la variabilidad del 
#  proceso es conocida (sigma=0.05)
# Datos de la pesta?a "d8.4" de Excel
dp84 <- read_excel("Taller_2/dtc_taller2_060318.xlsx", sheet = "d8.4")
ccdp84 <- cusum(data=dp84[,2],sizes=1,center=8.02,
                std.dev=0.05,decision.interval=4.77,
                se.shift=0.5,plot=T,
                title="Carta CUSUM para monitorear la media del proceso")
# Al revisar la carta, se observa que el proceso est?
#  bajo control.
##### Punto 8.6.
# Realizar una carta CUSUM FIR suponiendo ahora que la 
#  l?nea central es 8
ccdp86 <- cusum(data=dp84[,2],sizes=1,center=8,
                std.dev=0.05,decision.interval=4.77,
                se.shift=0.5,head.start=(4.77/2),plot=T,
                title="Carta CUSUM para monitorear la media del proceso")
##### Punto 8.17
cedp817 <- ewma(data=dp84[,2],sizes=1,center=8.04,
                std.dev=0.05,lambda=0.2,
                nsigmas=3,plot=T,
                title="Carta EWMA para monitorear
                la media del proceso")
#################################################################