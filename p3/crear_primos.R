setwd("C:/Users/ASUS/Desktop/Asignaturas/Computo paralelo")
suppressMessages(library(doParallel))
nucleos<-(detectCores()-1)
conteos <- function(n) {
  if (n == 1 || n == 2) {
    return("Trivial")
  }
  if (n %% 2 == 0) {
    return("Par")
  }
  for (i in seq(3, max(3, ceiling(sqrt(n))), 2)) {
    if ((n %% i) == 0) {
      return("Impar no primo")
    }
  }
  return("Impar primo")
}
desde <- 10000
hasta <-  30000
original <- desde:hasta
registerDoParallel(makeCluster(nucleos))
ot<-numeric()
ot <- foreach(n = original, .combine=c) %dopar% conteos(n) # de menor a mayor
stopImplicitCluster()
table(ot)

#Ahora ordenamos los elementos del vector tanto en dificultad
#creciente como en decreciente

dif_crec_t<-numeric()
dif_dec_t<-numeric()
tipos<-c("Trivial","Par","Impar no primo","Impar primo")
k<-1
for (i in tipos)
{
  for (j in desde:hasta)
  { 
    if (ot[j-desde+1]==i)
    {
      dif_crec_t[k]<-j
      k<-k+1
    }
  }
}
dif_dec_t<-rev(dif_crec_t)
