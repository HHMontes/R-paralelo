library(ggplot2)
library(reshape2)
setwd("C:/Users/ASUS/Desktop/Asignaturas/Computo paralelo/p7")

g <- function(x, y) {
  return(((x + 0.5)^4 - 30 * x^2 - 20 * x + (y + 0.5)^4 - 30 * y^2 - 20 * y)/100)
}

png("p7_2d.png", width=700, height=700)
x <- seq(-6, 5, 0.25)
y <-  x
z <- outer(x, y, g)
persp(x, y, z, shade=0.2, col='orange', theta=40, phi=30)
graphics.off()

low <- -6
high <- 5
step <- 0.25
replicas <- 100

replica <- function(t) {
  library(reshape2)
  curr <- runif(2, low, high)
  best <- curr
  for (tiempo in 1:t) {
    
    delta_x <- runif(1, 0, step)
    delta_y <- runif(1, 0, step)
    
    x_l <- max(low,curr[1]-delta_x)
    x_r <- min(high,curr[1]+delta_x)
    
    y_b <- max(low,curr[2]-delta_y)
    y_t <- min(high,curr[2]+delta_y)

    gs <- outer(c(x_l,x_r),c(y_b,y_t),g)
    dimnames(gs)<-list(c(x_l,x_r),c(y_b,y_t))
    gsd<-melt(gs)
    curr<-as.numeric(gsd[which.max(gs),1:2])
    if (g(curr[1],curr[2]) > g(best[1],best[2])) {
      best <- curr
    }
  }
  return(c(best[1],best[2],g(best[1],best[2])))
}
suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
#x <- seq(low, high, length.out=500)
#y <- foreach(i = x, .combine=c) %dopar% f(i)

for (pot in 1:4)
{
  tmax <- 10^pot
  resultados <- as.matrix(foreach(i = 1:replicas, .combine=c) %dopar% replica(tmax))
  xs<-resultados[seq(from=1,to=(3*replicas),3)]
  ys<-resultados[seq(from=2,to=(3*replicas),3)]
  valores<-resultados[seq(from=3,to=(3*replicas),3)]
  d0<-data.frame(x=xs,y=ys,z=valores)
  x <- seq(-6, 5, 0.25)
  y <-  x
  z <- outer(x, y, g)
  dimnames(z) <- list(x, y)
  d <- melt(z)
  names(d) <- c("x1", "y1", "z1")
  
  p<-ggplot()+geom_tile(data=d,aes(x=x1,y=y1,fill=z1))+
    geom_point(data=d0,aes(x=x,y=y,colour="red"))+xlab("Coordenada x")+
    ylab("Coordenada y")+
  png(paste("p7_", tmax, ".png", sep=""), width=700, height=300)
  print(p)
  graphics.off()
}
stopImplicitCluster()