library(parallel)
library(ggplot2)
library(gridExtra)
library(xtable)
setwd("C:/Users/ASUS/Desktop/Asignaturas/Computo paralelo/p6")

#Ajusto datos de inicio

l <- 1.5
n <- 50
pi <- 0.05
v <- l/30
pr<-0.02
pv<-0
tmax <- 100
r<-0.1

probabilidades<-c(1-pi-pv,pi,pv)
tipos<-c("S","I","R")
maxi<-c(10,20,30,40)

resultados<-data.frame(Replicas=integer(),
                       Version=factor(),
                       Tiempos=double())

for (t in maxi)
{
  for (p in 1:t)
  {
    ti<-Sys.time()
    #Creo estado inicial
    
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
    tf<-Sys.time()
    tiempos_p=as.numeric(tf-ti)
    resultados<-rbind(resultados,data.frame(Replicas=t,
                           Version="Paralelo",
                           Tiempos=tiempos_p))
  }
}
png("Tiempos_par.png",width = 617, height = 354)
print(plot1<-ggplot(resultados, aes(x=as.factor(Replicas),
                              y=Tiempos,fill=Replicas)) + 
  geom_boxplot(alpha=0.3)+theme(legend.position="none")+
  labs(x = "Cantidad de R\u{E9}plicas",y = "Tiempos computacionales")+
  theme(legend.position="bottom")+
  labs(fill = "R\u{E9}plicas"))
graphics.off()


library(parallel)
library(ggplot2)
library(gridExtra)
library(xtable)
setwd("C:/Users/ASUS/Desktop/Asignaturas/Computo paralelo/p6")

l <- 1.5
n <- 50
pi <- 0.05
pr <- 0.02
v <- l / 30
r <- 0.1
tmax <- 100

for (t in maxi)
{
  for (p in 1:t)
  {
for (p in 1:10)
{
  ti<-Sys.time()  
  agentes <- data.frame(x = double(), y = double(), dx = double(), dy = double(), estado  = character())
  for (i in 1:n) {
    e <- "S"
    if (runif(1) < pi) {
      e <- "I"
    }
    agentes <- rbind(agentes, data.frame(x = runif(1, 0, l), y = runif(1, 0, l),
                                         dx = runif(1, -v, v), dy = runif(1, -v, v),
                                         estado = e))
    levels(agentes$estado) <- c("S", "I", "R")
  }
  epidemia <- integer()
  for (tiempo in 1:tmax) {
    infectados <- dim(agentes[agentes$estado == "I",])[1]
    epidemia <- c(epidemia, infectados)
    if (infectados == 0) {
      break
    }
    contagios <- rep(FALSE, n)
    for (i in 1:n) { # posibles contagios
      a1 <- agentes[i, ]
      if (a1$estado == "I") { # desde los infectados
        for (j in 1:n) {
          if (!contagios[j]) { # aun sin contagio
            a2 <- agentes[j, ]
            if (a2$estado == "S") { # hacia los susceptibles
              dx <- a1$x - a2$x
              dy <- a1$y - a2$y
              d <- sqrt(dx^2 + dy^2)
              if (d < r) { # umbral
                p <- (r - d) / r
                if (runif(1) < p) {
                  contagios[j] <- TRUE
                }
              }
            }
          }
        }
      }
    }
    for (i in 1:n) { # movimientos y actualizaciones
      a <- agentes[i, ]
      if (contagios[i]) {
        a$estado <- "I"
      } else if (a$estado == "I") { # ya estaba infectado
        if (runif(1) < pr) {
          a$estado <- "R" # recupera
        }
      }
      a$x <- a$x + a$dx
      a$y <- a$y + a$dy
      if (a$x > l) {
        a$x <- a$x - l
      }
      if (a$y > l) {
        a$y <- a$y - l
      }
      if (a$x < 0) {
        a$x <- a$x + l
      }
      if (a$y < 0) {
        a$y <- a$y + l
      }
      agentes[i, ] <- a
    }
  }
  tf<-Sys.time()
  tiempos<-as.numeric(tf-ti)
  resultados<-rbind(resultados,data.frame(Replicas=t,
                                          Version="Paralelo",
                                          Tiempos=tiempos))
}
  }
}
png("Tiempos_par_sec.png",width = 617, height = 354)
print(plot1<-ggplot(resultados, aes(x=as.factor(Replicas),
                                    y=Tiempos,fill=Version)) + 
        geom_boxplot(alpha=0.3)+theme(legend.position="none")+
        labs(x = "Cantidad de R\u{E9}plicas",y = "Tiempos computacionales")+
        theme(legend.position="bottom")+
        labs(fill = "Versi\u{F3}n"))
graphics.off()