library(magick)
library(ggplot2)
library(parallel)
setwd("C:/Users/ASUS/Desktop/Asignaturas/Computo paralelo/P10")
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
n<-50
tmax<-100
  #Creamos las posiciones iniciales de los objetos y sus atributos
  
  #Posiciones para cargas normales con masas normales

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
  
  #Establecemos parametros para el experimento
  eps <- 0.001
  
  #Exportamos valores iniciales al cluster
  
  cluster<-makeCluster(detectCores(logical = F))
  clusterExport(cluster,"n")
  clusterExport(cluster,"eps")
  clusterExport(cluster,"digitos")
  clusterExport(cluster,"p")
  
  #Creamos matriz para guardar desplazamientos
  
  desplazamientos_1<-matrix(ncol=tmax,nrow=n)
  delta_x <- matrix(ncol=tmax,nrow=n)
  delta_y <- matrix(ncol=tmax,nrow=n)
  
  #Iniciamos la experimentación sobre la configuración inicial
  
  clusterExport(cluster,"p")
  for (iter in 1:tmax) {
    clusterExport(cluster,"fuerza")
    f<-parSapply(cluster,1:n,fuerza)
    mag_f<-sqrt(f[1,]^2+f[2,]^2)
    delta <- 0.05 / max(mag_f)# que nadie desplace una paso muy largo
    clusterExport(cluster,"delta")
    clusterExport(cluster,"f")
    delta_x[,iter] <- parSapply(cluster,1:n,function(i){(1/p[i,]$m)*delta * f[1,i]}) 
    delta_y[,iter] <- parSapply(cluster,1:n,function(i){(1/p[i,]$m)*delta * f[2,i]})
    desplazamientos_1[,iter] <- sqrt(delta_x[,iter]^2+delta_y[,iter]^2) 
    clusterExport(cluster,"delta_x")
    clusterExport(cluster,"delta_y")
    p$x <- parSapply(cluster,1:n,function(i){max(min(p[i,]$x+delta_x[i],1),0)})
    p$y <- parSapply(cluster,1:n,function(i){max(min(p[i,]$y+delta_y[i],1),0)})
    clusterExport(cluster,"p")
  }
  d_1<-apply(desplazamientos_1,1,mean)
  p_1<-p
  
  #Posiciones para cargas binarias con masas normales
  
  p <- data.frame(x = rnorm(n), y=rnorm(n), c=sample(c(-1,1),n,replace = TRUE),
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
  
  #Establecemos parametros para el experimento
  eps <- 0.001
  
  #Exportamos valores iniciales al cluster
  
  cluster<-makeCluster(detectCores(logical = F))
  clusterExport(cluster,"n")
  clusterExport(cluster,"eps")
  clusterExport(cluster,"digitos")
  clusterExport(cluster,"p")
  
  #Creamos matriz para guardar desplazamientos
  
  desplazamientos_2<-matrix(ncol=tmax,nrow=n)
  delta_x <- matrix(ncol=tmax,nrow=n)
  delta_y <- matrix(ncol=tmax,nrow=n)
  
  #Iniciamos la experimentación sobre la configuración inicial
  
  clusterExport(cluster,"p")
  for (iter in 1:tmax) {
    clusterExport(cluster,"fuerza")
    f<-parSapply(cluster,1:n,fuerza)
    mag_f<-sqrt(f[1,]^2+f[2,]^2)
    delta <- 0.05 / max(mag_f)# que nadie desplace una paso muy largo
    clusterExport(cluster,"delta")
    clusterExport(cluster,"f")
    delta_x[,iter] <- parSapply(cluster,1:n,function(i){(1/p[i,]$m)*delta * f[1,i]}) 
    delta_y[,iter] <- parSapply(cluster,1:n,function(i){(1/p[i,]$m)*delta * f[2,i]})
    desplazamientos_2[,iter] <- sqrt(delta_x[,iter]^2+delta_y[,iter]^2) 
    clusterExport(cluster,"delta_x")
    clusterExport(cluster,"delta_y")
    p$x <- parSapply(cluster,1:n,function(i){max(min(p[i,]$x+delta_x[i],1),0)})
    p$y <- parSapply(cluster,1:n,function(i){max(min(p[i,]$y+delta_y[i],1),0)})
    clusterExport(cluster,"p")
  }
  
  #Comparaciones para normales con y sin carga controlada
  
  d_2<-apply(desplazamientos_2,1,mean)
  p_2<-p
  png(paste("Normales.png", sep=""), width=700, height=300)
  par(mfcol=c(1,2))
  plot((1/p_1$m),d_1,main="Cargas normales",
       xlab="Reciproco de la masa (1/m)", ylab="Dezplazamientos medios")
  plot((1/p_2$m),d_2,main="Cargas binarias",
               xlab="Reciproco de la masa (1/m)", ylab="Dezplazamientos medios")
  graphics.off()
  
  #Posiciones para cargas normales con masas gamma
  
  p <- data.frame(x = rnorm(n), y=rnorm(n), c=rnorm(n),
                  m=rgamma(n,shape =1 ,scale =2 ))
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
  
  #Establecemos parametros para el experimento
  eps <- 0.001
  
  #Exportamos valores iniciales al cluster
  
  cluster<-makeCluster(detectCores(logical = F))
  clusterExport(cluster,"n")
  clusterExport(cluster,"eps")
  clusterExport(cluster,"digitos")
  clusterExport(cluster,"p")
  
  #Creamos matriz para guardar desplazamientos
  
  desplazamientos_3<-matrix(ncol=tmax,nrow=n)
  delta_x <- matrix(ncol=tmax,nrow=n)
  delta_y <- matrix(ncol=tmax,nrow=n)
  
  #Iniciamos la experimentación sobre la configuración inicial
  
  clusterExport(cluster,"p")
  for (iter in 1:tmax) {
    clusterExport(cluster,"fuerza")
    f<-parSapply(cluster,1:n,fuerza)
    mag_f<-sqrt(f[1,]^2+f[2,]^2)
    delta <- 0.05 / max(mag_f)# que nadie desplace una paso muy largo
    clusterExport(cluster,"delta")
    clusterExport(cluster,"f")
    delta_x[,iter] <- parSapply(cluster,1:n,function(i){(1/p[i,]$m)*delta * f[1,i]}) 
    delta_y[,iter] <- parSapply(cluster,1:n,function(i){(1/p[i,]$m)*delta * f[2,i]})
    desplazamientos_3[,iter] <- sqrt(delta_x[,iter]^2+delta_y[,iter]^2) 
    clusterExport(cluster,"delta_x")
    clusterExport(cluster,"delta_y")
    p$x <- parSapply(cluster,1:n,function(i){max(min(p[i,]$x+delta_x[i],1),0)})
    p$y <- parSapply(cluster,1:n,function(i){max(min(p[i,]$y+delta_y[i],1),0)})
    clusterExport(cluster,"p")
  }
  d_3<-apply(desplazamientos_3,1,mean)
  p_3<-p

  #Posiciones para cargas binarias con masas gamma
  
  p <- data.frame(x = rnorm(n), y=rnorm(n), c=sample(c(-1,1),n,replace = TRUE),
                  m=rgamma(n,shape =1 ,scale =2 ))
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
  
  #Establecemos parametros para el experimento
  eps <- 0.001
  
  #Exportamos valores iniciales al cluster
  
  cluster<-makeCluster(detectCores(logical = F))
  clusterExport(cluster,"n")
  clusterExport(cluster,"eps")
  clusterExport(cluster,"digitos")
  clusterExport(cluster,"p")
  
  #Creamos matriz para guardar desplazamientos
  
  desplazamientos_4<-matrix(ncol=tmax,nrow=n)
  delta_x <- matrix(ncol=tmax,nrow=n)
  delta_y <- matrix(ncol=tmax,nrow=n)
  
  #Iniciamos la experimentación sobre la configuración inicial
  
  clusterExport(cluster,"p")
  for (iter in 1:tmax) {
    clusterExport(cluster,"fuerza")
    f<-parSapply(cluster,1:n,fuerza)
    mag_f<-sqrt(f[1,]^2+f[2,]^2)
    delta <- 0.05 / max(mag_f)# que nadie desplace una paso muy largo
    clusterExport(cluster,"delta")
    clusterExport(cluster,"f")
    delta_x[,iter] <- parSapply(cluster,1:n,function(i){(1/p[i,]$m)*delta * f[1,i]}) 
    delta_y[,iter] <- parSapply(cluster,1:n,function(i){(1/p[i,]$m)*delta * f[2,i]})
    desplazamientos_4[,iter] <- sqrt(delta_x[,iter]^2+delta_y[,iter]^2) 
    clusterExport(cluster,"delta_x")
    clusterExport(cluster,"delta_y")
    p$x <- parSapply(cluster,1:n,function(i){max(min(p[i,]$x+delta_x[i],1),0)})
    p$y <- parSapply(cluster,1:n,function(i){max(min(p[i,]$y+delta_y[i],1),0)})
    clusterExport(cluster,"p")
  }
  
  #Comparaciones para normales con y sin carga controlada
  
  d_4<-apply(desplazamientos_4,1,mean)
  p_4<-p
  png(paste("Gamma.png", sep=""), width=700, height=300)
  par(mfcol=c(1,2))
  plot((1/p_3$m),d_3,main="Cargas normales",
       xlab="Reciproco de la masa (1/m)", ylab="Dezplazamientos medios")
  plot((1/p_4$m),d_4,main="Cargas binarias",
       xlab="Reciproco de la masa (1/m)", ylab="Dezplazamientos medios")
  graphics.off()
  
  
  #Posiciones para cargas normales con masas beta
  
  p <- data.frame(x = rnorm(n), y=rnorm(n), c=rnorm(n),
                  m=rbeta(n,shape1 =5 ,shape2 =1 ))
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
  
  #Establecemos parametros para el experimento
  eps <- 0.001
  
  #Exportamos valores iniciales al cluster
  
  cluster<-makeCluster(detectCores(logical = F))
  clusterExport(cluster,"n")
  clusterExport(cluster,"eps")
  clusterExport(cluster,"digitos")
  clusterExport(cluster,"p")
  
  #Creamos matriz para guardar desplazamientos
  
  desplazamientos_5<-matrix(ncol=tmax,nrow=n)
  delta_x <- matrix(ncol=tmax,nrow=n)
  delta_y <- matrix(ncol=tmax,nrow=n)
  
  #Iniciamos la experimentación sobre la configuración inicial
  
  clusterExport(cluster,"p")
  for (iter in 1:tmax) {
    clusterExport(cluster,"fuerza")
    f<-parSapply(cluster,1:n,fuerza)
    mag_f<-sqrt(f[1,]^2+f[2,]^2)
    delta <- 0.05 / max(mag_f)# que nadie desplace una paso muy largo
    clusterExport(cluster,"delta")
    clusterExport(cluster,"f")
    delta_x[,iter] <- parSapply(cluster,1:n,function(i){(1/p[i,]$m)*delta * f[1,i]}) 
    delta_y[,iter] <- parSapply(cluster,1:n,function(i){(1/p[i,]$m)*delta * f[2,i]})
    desplazamientos_5[,iter] <- sqrt(delta_x[,iter]^2+delta_y[,iter]^2) 
    clusterExport(cluster,"delta_x")
    clusterExport(cluster,"delta_y")
    p$x <- parSapply(cluster,1:n,function(i){max(min(p[i,]$x+delta_x[i],1),0)})
    p$y <- parSapply(cluster,1:n,function(i){max(min(p[i,]$y+delta_y[i],1),0)})
    clusterExport(cluster,"p")
  }
  d_5<-apply(desplazamientos_5,1,mean)
  p_5<-p
  
  #Posiciones para cargas binarias con masas gamma
  
  p <- data.frame(x = rnorm(n), y=rnorm(n), c=sample(c(-1,1),n,replace = TRUE),
                  m=rbeta(n,shape1 =5 ,shape2 =1 ))
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
  
  #Establecemos parametros para el experimento
  eps <- 0.001
  
  #Exportamos valores iniciales al cluster
  
  cluster<-makeCluster(detectCores(logical = F))
  clusterExport(cluster,"n")
  clusterExport(cluster,"eps")
  clusterExport(cluster,"digitos")
  clusterExport(cluster,"p")
  
  #Creamos matriz para guardar desplazamientos
  
  desplazamientos_6<-matrix(ncol=tmax,nrow=n)
  delta_x <- matrix(ncol=tmax,nrow=n)
  delta_y <- matrix(ncol=tmax,nrow=n)
  
  #Iniciamos la experimentación sobre la configuración inicial
  
  clusterExport(cluster,"p")
  for (iter in 1:tmax) {
    clusterExport(cluster,"fuerza")
    f<-parSapply(cluster,1:n,fuerza)
    mag_f<-sqrt(f[1,]^2+f[2,]^2)
    delta <- 0.05 / max(mag_f)# que nadie desplace una paso muy largo
    clusterExport(cluster,"delta")
    clusterExport(cluster,"f")
    delta_x[,iter] <- parSapply(cluster,1:n,function(i){(1/p[i,]$m)*delta * f[1,i]}) 
    delta_y[,iter] <- parSapply(cluster,1:n,function(i){(1/p[i,]$m)*delta * f[2,i]})
    desplazamientos_6[,iter] <- sqrt(delta_x[,iter]^2+delta_y[,iter]^2) 
    clusterExport(cluster,"delta_x")
    clusterExport(cluster,"delta_y")
    p$x <- parSapply(cluster,1:n,function(i){max(min(p[i,]$x+delta_x[i],1),0)})
    p$y <- parSapply(cluster,1:n,function(i){max(min(p[i,]$y+delta_y[i],1),0)})
    clusterExport(cluster,"p")
  }
  
  #Comparaciones para normales con y sin carga controlada
  
  d_6<-apply(desplazamientos_6,1,mean)
  p_6<-p
  png(paste("Beta.png", sep=""), width=700, height=300)
  par(mfcol=c(1,2))
  plot((1/p_5$m),d_5,main="Cargas normales",
       xlab="Reciproco de la masa (1/m)", ylab="Dezplazamientos medios")
  plot((1/p_6$m),d_6,main="Cargas binarias",
       xlab="Reciproco de la masa (1/m)", ylab="Dezplazamientos medios")
  graphics.off()
  
  modelo_1_1<-lm((1/p_1$m)~d_1)
  summary(modelo_1_1)
  modelo_1_2<-lm((1/p_2$m)~d_2)
  summary(modelo_1_2)
  
  modelo_2_1<-lm((1/p_3$m)~d_3)
  summary(modelo_2_1)
  modelo_2_2<-lm((1/p_4$m)~d_4)
  summary(modelo_2_2)
  
  modelo_3_1<-lm((1/p_5$m)~d_5)
  summary(modelo_3_1)
  modelo_3_2<-lm((1/p_6$m)~d_6)
  summary(modelo_3_2)