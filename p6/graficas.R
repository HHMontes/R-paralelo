#Las presentes funciones apoyan la tarea gr\u{E1}fica para el modelo SIR
#Sin embargo, no corren independientes del c\u{F3}digo principal, pues requiere
#de los objetos que este produce. Se pone acá separado del cuerpo principal, 
#s\u{F3}lo para referencia.


p_sus<-epidemia[,"Susceptibles"] / n
p_inf<-epidemia[,"Infectados"] / n
p_rec<-epidemia[,"Recuperados"] / n
datos<-data.frame(x=rep(seq(0,tmax,1),3),y=c(p_sus,p_inf,p_rec),
                  Estado=c(rep("Susceptibles",tmax+1),rep("Infectados",tmax+1),
                           rep("Recuperados",tmax+1)))
png("p6e.png", width=600, height=300)
ggplot(data=datos,
       aes(x=x, y=y, colour=Estado)) +
  geom_line()+
  labs(x = "Tiempo",y = "Porcentajes por estados")
graphics.off()
clusterExport(cluster,"m_estados")
clusterExport(cluster,"digitos")
clusterExport(cluster,"l")
parSapply(cluster,0:tmax,function(t){
  aS_1 <- valores[rep(m_estados[,t+1]=="S",2),t+1]
  naS_1<-length(aS_1)
  aI_1 <- valores[rep(m_estados[,t+1]=="I",2),t+1]
  naI_1<-length(aI_1)
  aR_1<- valores[rep(m_estados[,t+1]=="R",2),t+1]
  naR_1<-length(aR_1)
  tl <- paste(t, "", sep="")
  while (nchar(tl) < digitos) {
    tl <- paste("0", tl, sep="")
  }
  salida <- paste("p6_t", tl, ".png", sep="")
  tiempo <- paste("Paso", t)
  png(salida)
  plot(l, type="n", main=tiempo, xlim=c(0, l), ylim=c(0, l), xlab="x", ylab="y")
  if (naS_1 > 0) {
    points(aS_1[1:(naS_1/2)],aS_1[((naS_1/2)+1):naS_1], pch=15, col="chartreuse3", bg="chartreuse3")
  }
  if (naI_1> 0) {
    points(aI_1[1:(naI_1/2)],aI_1[((naI_1/2)+1):naI_1], pch=16, col="firebrick2", bg="firebrick2")
  }
  if (naR_1> 0) {
    points(aR_1[1:(naR_1/2)],aR_1[((naR_1/2)+1):naR_1], pch=17, col="goldenrod", bg="goldenrod")
  }
  graphics.off()
})