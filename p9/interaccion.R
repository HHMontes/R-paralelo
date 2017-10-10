library(magick)
library(ggplot2)
library(parallel)
setwd("C:/Users/ASUS/Desktop/Asignaturas/Computo paralelo/P10")
n <- 50
p <- data.frame(x = rnorm(n), y=rnorm(n), c=rnorm(n),
                m=rnorm(n))
xmax <- max(p$x)
xmin <- min(p$x)
p$x <- (p$x - xmin) / (xmax - xmin) # ahora son de 0 a 1
ymax <- max(p$y)
ymin <- min(p$y)
p$y <- (p$y - ymin) / (ymax - ymin) # las y tambien
cmax <- max(p$c)
cmin <- min(p$c)
p$c <- 2 * (p$c - cmin) / (cmax - cmin) - 1 # cargas son entre -1 y 1
mmax <- max(p$m)
mmin <- min(p$m)
p$m <- 1+5*((p$m-mmin)/(mmax-mmin)) # las masas entre 1 y 2
factor_radio<-0.015/max(p$m)
p$r <- factor_radio*p$m
rmax <- max(p$r)
rmin <- min(p$r)
library(lattice)
p$g <- round(5 * p$c) # coloreamos segun la carga a 11 niveles de -5 a 5
paso <- floor(256 / 10)
niveles <- seq(0, 255, paso)
colores <- rgb(niveles, rep(0, 11), rev(niveles), max=255)
eps <- 0.001
tmax <- 100
replicas <- 1 
digitos <- floor(log(tmax, 10)) + 1
tl <- "0"
while (nchar(tl) < digitos) {
  tl <- paste("0", tl, sep="")
}
png(paste("p9_t", tl, ".png", sep=""), width=700, height=300)
print(with(p, symbols(x=x, y=y, circles=r, inches=FALSE,
                      ann=T, bg=colores[p$g+6], fg=NULL,xlim =c(-0.1,1),
                      ylim =c(-0.1,1),main = "Paso 000")))
graphics.off()
cluster<-makeCluster(detectCores(logical = F))
clusterExport(cluster,"n")
clusterExport(cluster,"eps")
clusterExport(cluster,"digitos")
clusterExport(cluster,"p")
desplazamientos<-matrix(ncol=tmax,nrow=n)
delta_x <- matrix(ncol=tmax,nrow=n)
delta_y <- matrix(ncol=tmax,nrow=n)
fuerza<-function(i) {
  xi <- p[i,]$x
  yi <- p[i,]$y
  ci <- p[i,]$c
  fx <- 0
  fy <- 0
  for (j in 1:n) {
    cj <- p[j,]$c
    dir <- (-1)^(1 + 1 * (ci * cj < 0))
    dx <- xi - p[j,]$x
    dy <- yi - p[j,]$y
    factor <- dir * abs(ci - cj) / (sqrt(dx^2 + dy^2) + eps)
    fx <- fx - dx * factor
    fy <- fy - dy * factor
  }
  return(c(fx, fy))
}
  
  clusterExport(cluster,"p")
  for (iter in 1:tmax) {#iter=1
    clusterExport(cluster,"fuerza")
    f<-parSapply(cluster,1:n,fuerza)
    mag_f<-sqrt(f[1,]^2+f[2,]^2)
    delta <- 0.05 / max(mag_f)# que nadie desplace una paso muy largo
    clusterExport(cluster,"delta")
    clusterExport(cluster,"f")
    delta_x[,iter] <- parSapply(cluster,1:n,function(i){(1/p[i,]$m)*delta * f[1,i]}) 
    delta_y[,iter] <- parSapply(cluster,1:n,function(i){(1/p[i,]$m)*delta * f[2,i]})
    desplazamientos[,iter] <- sqrt(delta_x[,iter]^2+delta_y[,iter]^2) 
    clusterExport(cluster,"delta_x")
    clusterExport(cluster,"delta_y")
    p$x <- parSapply(cluster,1:n,function(i){max(min(p[i,]$x+delta_x[i],1),0)})
    p$y <- parSapply(cluster,1:n,function(i){max(min(p[i,]$y+delta_y[i],1),0)})
    clusterExport(cluster,"p")
    tl <- paste(iter,"", sep="")
    while (nchar(tl) < digitos) {
      tl <- paste("0", tl, sep="")
    }
    png(paste("p9_t", tl, ".png", sep=""), width=700, height=300)
    print(with(p, symbols(x=x, y=y, circles=r, inches=FALSE,
                          ann=T, bg=colores[p$g+6],fg=NULL,xlim = c(-0.1,1),
                          ylim = c(-0.1,1),main=paste("Paso",tl,sep = " "))))
    #print(ggplot(data=p,aes(x=x,y=y,size=r,col=colores[p$g+6]))+
    #  geom_point(show.legend= FALSE)+xlab("Coordenada x")+
    #  ylab("Coordenada y")+xlim(c(-0.1,1))+ylim(c(-0.1,1))+
    #  ggtitle(paste("Paso ", iter)))
    graphics.off()
  }
  stopCluster(cluster)
  ab1<-character(length = tmax)
  for (i in 1:tmax){
    ab1[i] <- paste(i,"", sep="")
    while (nchar(ab1[i]) < digitos) {
      ab1[i] <- paste("0", ab1[i], sep="")
    }  
  }
  paste("p9_t",ab1,".png",sep="")
  frames=lapply(1:tmax,function(x) image_read(paste("p9_t",ab1[x],".png",sep="")))
  animation <- image_animate(image_join(frames),fps=5)
  print(animation)
  w3<-paste("P9_N_4", ".gif"  )
  image_write(animation, w3)
  medias<-apply(desplazamientos,1,mean)
  modelo<-lm((1/p$m)~medias)
  summary(modelo)
  plot((1/p$m),medias)  