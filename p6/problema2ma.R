library(parallel)
library(ggplot2)
library(gridExtra)
library(xtable)
setwd("C:/Users/ASUS/Desktop/Asignaturas/Computo paralelo/p6")

#Ajusto datos de inicio

l <- 1.5
n <- 50
pi <-seq(0.00,0.10,0.01)
v <- l/30
pr<-0.02
pv<-seq(0.00,0.40,0.05)
tmax <- 100
r<-0.1

for (i in 1:length(pv))
{
  for (j in 1:length(pi))
  
  {
    probabilidades<-c(1-pi[j]-pv[i],pi[j],pv[i])
    tipos<-c("S","I","R")
    agentes<-data.frame(x = runif(n, 0, l), y = runif(n, 0, l),
                        dx = runif(n, -v, v), dy = runif(n, -v, v),
                        estado = sample(tipos,n,replace = T,probabilidades))
    
    #Construyo movimientos en paralelo y los guardo en matriz valores
    
    cluster<-makeCluster(detectCores(logical = F))
    clusterExport(cluster,"agentes")
    clusterExport(cluster,"l")
    clusterExport(cluster,"r")
    clusterExport(cluster,"pr")
    valores<-parSapply(cluster,0:tmax,function(t){
      cbind((agentes$x+(t*agentes$dx))-(floor((agentes$x+(t*agentes$dx))/l)*l),
            (agentes$y+(t*agentes$dy))-(floor((agentes$y+(t*agentes$dy))/l)*l))
    })
    
    #Construyo una matriz epidemia que contenga los conteos de los tres estados
    
    clusterExport(cluster,"valores")
    epidemia <- matrix(ncol=3,nrow=(tmax+1))
    colnames(epidemia)<-c("Susceptibles","Infectados","Recuperados")
    digitos <- floor(log(tmax, 10)) + 1
    m_estados<-matrix(agentes$estado,ncol=tmax+2,nrow = n)
    epidemia[1,"Susceptibles"]<-length(agentes$estado[agentes$estado == "S"])
    epidemia[1,"Infectados"]<-length(agentes$estado[agentes$estado == "I"])
    epidemia[1,"Recuperados"]<-length(agentes$estado[agentes$estado == "R"])
    
    #Inicio el ciclo que recorre el tiempo
    
    for (tiempo in 0:(tmax-1)) {
      if (epidemia[(tiempo+1),"Infectados"]== 0) {
        break
      }
      
      #Paralelizo contagios y actualizaciones
      
      clusterExport(cluster,"tiempo")
      clusterExport(cluster,"agentes")
      estados_u<-as.character(agentes$estado)
      clusterExport(cluster,"estados_u")
      agentes$estado<-parSapply(cluster,1:n,function(i){
        a<-estados_u[i]
        if (estados_u[i]=="I" & runif(1) < pr)
        {
          a<-"R"
        }
        if (estados_u[i]=="S")
        {
          for (j in which(estados_u=="I"))
          {
            difx <- valores[i,tiempo+1]-valores[j,tiempo+1]
            dify <- valores[i+50,tiempo+1]-valores[j+50,tiempo+1]
            d <- sqrt(difx^2 + dify^2)
            if (runif(1)<((r-d)/r) & d<r)
            {
              a<-"I"
              break
            }  
          }
        }
        return(a)
      }
      )
      
      #Creo objetos de resultados, \u{FA}tiles en las labores de graficaci\u{F3}n
      
      m_estados[,tiempo+2]<-agentes$estado
      epidemia[tiempo+2,"Susceptibles"] <-length(agentes$estado[agentes$estado == "S"])
      epidemia[tiempo+2,"Infectados"] <- length(agentes$estado[agentes$estado == "I"])
      epidemia[tiempo+2,"Recuperados"] <- length(agentes$estado[agentes$estado == "R"])
    }
    stopCluster(cluster)
  }
}

