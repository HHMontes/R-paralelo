library(ggplot2)
library(reshape2)
setwd("C:/Users/ASUS/Desktop/Asignaturas/Computo paralelo/p7")

g <- function(x, y) {
  return(((x + 0.5)^4 - 30 * x^2 - 20 * x + (y + 0.5)^4 - 30 * y^2 - 20 * y)/100)
}

low <- -6
high <- 5
step <- 0.25
replicas <- 1000
reduc<-0.01
curr <- runif(2, low, high)
best <- curr
vecino <- curr
g_curr<-g(curr[1],curr[2])
g_best<-g_curr
g_vecino<-g_curr

message("It\tMejor\tActual\tVecino\tTemperatura")
message(sprintf("%i\t%.4f\t%.4f\t%.4f\t%.4f", 0L, g_best, g_curr, g_vecino, 1))

for (tiempo in 1:replicas)
{
  Temperatura<-(1-reduc)^tiempo
  delta_x <- runif(1, -step, step)
  delta_y <- runif(1, -step, step)

  vecino<-curr+c(delta_x,delta_y)
  if (vecino[1]<low)
  {
    vecino[1]<-low
  }
  if (vecino[1]>high)
  {
    vecino[1]<-high
  }
  if (vecino[2]<low)
  {
    vecino[2]<-low
  }
  if (vecino[2]>high)
  {
    vecino[2]<-high
  }
  g_vecino<-g(vecino[1],vecino[2])
  delta_ob<-g_vecino-g_curr
    
  if ((delta_ob > 0 || runif(1,0,1)<exp(delta_ob/Temperatura)))
  {
    curr <- vecino
    g_curr <- g_vecino
  }
  
  if (g_vecino > g_best) {
    best <- vecino
    g_best <- g_vecino        
  }
  message(sprintf("%i\t%.4f\t%.4f\t%.4f\t%.4f", tiempo, g_best, g_curr, g_vecino, Temperatura))
}
print(list(iteracion = replicas, mejor_valor = g_best, mejor_solucion = best))
