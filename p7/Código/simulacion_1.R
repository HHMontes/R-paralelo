library(ggplot2)
library(reshape2)
setwd("C:/Users/ASUS/Desktop/Asignaturas/Computo paralelo/p7")

g <- function(x, y) {
  return(((x + 0.5)^4 - 30 * x^2 - 20 * x + (y + 0.5)^4 - 30 * y^2 - 20 * y)/100)
}

low <- -6
high <- 5
step <- 0.1
t <- 100

x <- seq(-6, 5, 0.25)
y <-  x
z <- outer(x, y, g)
dimnames(z) <- list(x, y)
d <- melt(z)
names(d) <- c("x1", "y1", "z1")
library(reshape2)
curr <- runif(2,low,high)
best <- curr
digitos <- floor(log(t, 10)) + 1

for (tiempo in 1:t)
  {
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
    tl <- paste(tiempo, "", sep="")
    while (nchar(tl) < digitos) {
      tl <- paste("0", tl, sep="")
    }
    salida <- paste("p7_t", tl, ".png", sep="")
    tiempo <- paste("Paso", tiempo)
    d0<-data.frame(x=best[1],y=best[2])
    p<-ggplot()+geom_tile(data=d,aes(x=x1,y=y1,fill=z1))+
      geom_point(data=d0,aes(x=x,y=y))+xlab("Coordenada x")+
      ylab("Coordenada y")+geom_vline(aes(xintercept = best[1],colour="green"))+
      geom_hline(aes(yintercept = best[2],colour="green"))
    png(salida, width=500, height=400)
    print(p)
    graphics.off()
  }
library(RColorBrewer) ## Libreria de colores 
library(ggplot2)
library(parallel)

library(magick)

setwd("C:/Users/ASUS/Desktop/Asignaturas/Computo paralelo/p7")
ab<-seq(0:99)
ab1<-ifelse(ab<10,paste("00",ab, sep = ""),ifelse(ab<100,paste("0",ab, sep = ""),ab ))

paste("p7_t",ab1,".png",sep="")
frames=lapply(1:100,function(x) image_read(paste("p7_t",ab1[x],".png",sep="")))
animation <- image_animate(image_join(frames),fps=20 )
print(animation)
w3<-paste("P7_N_4", ".gif"  )
image_write(animation, w3)
