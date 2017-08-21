setwd("C:/Users/ASUS/Desktop/Asignaturas/Computo paralelo")
library(parallel)
dim <- 10
num <-  dim^2
punto_corte<-100
replicas<-10
prob<-seq(0,1,by=0.1)
tiempo_vida<-matrix(ncol=length(prob),nrow=replicas)
colnames(tiempo_vida)<-paste("Prob_ini=",prob)
ciclos<-matrix(ncol = length(prob),nrow =replicas)
for (k in 1:replicas)#k=1
{ 
  for (j in 1:length(prob))#j=7
  { 
    actual <- matrix((1*(runif(num)<=prob[j])), nrow=dim, ncol=dim)
    #suppressMessages(library("sna"))
    #png("p2_t0.png")
    #plot.sociomatrix(actual, diaglab=FALSE, main="Inicio")
    #graphics.off()
    paso <- function(pos)
    {
      fila <- floor((pos - 1) / dim) + 1
      columna <- ((pos - 1) %% dim) + 1
      vecindad <-  actual[max(fila - 1, 1) : min(fila + 1, dim),
                          max(columna - 1, 1): min(columna + 1, dim)]
      return(1 * ((sum(vecindad) - actual[fila, columna]) == 3))
    }
    cluster <- makeCluster(detectCores() - 1)
    clusterExport(cluster, "dim")
    clusterExport(cluster, "paso")
    clusterExport(cluster, "punto_corte")
    for (iteracion in 1:punto_corte)
    {
      clusterExport(cluster, "actual")
      siguiente <- parSapply(cluster, 1:num, paso)
      ciclos[k,j]=1
      tiempo_vida[k,j]=punto_corte
      if (sum(siguiente) == 0)
      { # todos murieron
        ciclos[k,j]=0
        tiempo_vida[k,j]=iteracion
        break
      }
      actual <- matrix(siguiente, nrow=dim, ncol=dim, byrow=TRUE)
      #salida = paste("p2_t", iteracion, ".png", sep="")
      #tiempo = paste("Paso", iteracion)
      #png(salida)
      #plot.sociomatrix(actual, diaglab=FALSE, main=tiempo)
      #graphics.off()
    }
  stopCluster(cluster)  
  }
}
