setwd("C:/Users/ASUS/Desktop/Asignaturas/Computo paralelo")
Vector<-read.csv("Vectores_dif.csv",sep=",",header = T)
matriz_v<-matrix(sample(Vector[,1]),nrow = 3,ncol = 6667,byrow = T)
suppressMessages(library(doParallel))
max_nucleos<-(detectCores()-1)
primo <- function(n) {
  p<-numeric(length = length(n))
  for(k in 1:length(n))
  {
    if (n[k] == 1 || n[k] == 2) {
      p[k]=1
    }
    if (n %% 2 == 0) {
      p[k]=0
    }
    for (i in seq(3, max(3, ceiling(sqrt(n))), 2)) {
      if ((i<n) && (n %% i) == 0) {
        p[k]=0
      }
    }
    p[k]=1  
  }
  return(p)
}
desde <- 1
hasta <- NROW(matriz_v)
original<-desde:hasta
registerDoParallel(max_nucleos)
replicas<-50
ot<-numeric()
for (r in 1:replicas) {
  ot<- c(ot, system.time(foreach(n = original, .combine=c) %dopar% primo(matriz_v[n,]))[3])
}
stopImplicitCluster()
summary(ot)