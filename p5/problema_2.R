library(ggplot2)
library(gridExtra)
library(xtable)
setwd("C:/Users/ASUS/Desktop/Asignaturas/Computo paralelo/p5")
f <- function(runs){
  xs <- runif(runs,min=-0.5,max=0.5)
  ys <- runif(runs,min=-0.5,max=0.5)
  in.circle <- xs^2 + ys^2 <= 0.5^2
  return((sum(in.circle)))
}
tam<-seq(100000,1000000,100000)
resultados_2<-data.frame(Tamano=factor(),
                       aproximado=double(),
                       Tiempos=double())
tiempos<-vector()
aproximado<-vector()
cuantos <- 500 #Distribuimos la generaci\u{F3}n de n\u{FA}meros en 500 pedazos
cluster<-makeCluster(detectCores(logical = F))
clusterExport(cluster,"f")
for (t in tam)
{ clusterExport(cluster,"t")
  for (i in 1:50)
  {
    clusterExport(cluster,"i")
    t0<-Sys.time()
    aproximado[i]<-sum(parSapply(
      cluster,1:cuantos,function(x) {f(t)}))*(4/(t*cuantos))
    tf<-Sys.time()
    tiempos[i]=(tf-t0)
  }
  resultados_2<-rbind(resultados_2,cbind(Tamano=t,aproximados=aproximado,Tiempo=tiempos))
}
stopCluster(cluster)
png("errores_pi.png",width = 617, height = 354)
plot1<-ggplot(resultados_2, aes(x=as.factor(Tamano),
                              y=aproximados,
                              fill=as.factor(Tamano)),xlab(Tamano)) + 
  geom_boxplot(alpha=0.3)+theme(legend.position="none")+
  geom_hline(yintercept = pi)+
  labs(x = "Tama\u{F1}o de muestra",y = "Valores aproximados")
plot2<-ggplot(resultados_2, aes(x=as.factor(Tamano),
                              y=abs(aproximados-pi)/pi,
                              fill=as.factor(Tamano)),xlab(Tamano)) + 
  geom_boxplot(alpha=0.3)+theme(legend.position="none")+
  labs(x = "Tama\u{F1}o de muestra",y = "Error relativo")
grid.arrange(plot1, plot2, nrow=1, ncol=2)
graphics.off()
png("tiempos_pi.png")
ggplot(resultados_2, aes(x=as.factor(Tamano),
                       y=Tiempo,
                       fill=as.factor(Tamano)),xlab(Tamano)) + 
  geom_boxplot(alpha=0.3)+theme(legend.position="none")+
  labs(x = "Tama\u{F1}o de muestra",y = "Tiempos de ejecuci\u{F3}n")
graphics.off()

test_1<-kruskal.test(abs(resultados_2$aproximados-pi)/pi~as.factor(resultados_2$Tamano))
test_2<-kruskal.test(resultados_2$Tiempo~as.factor(resultados_2$Tamano))
Tabla<-as.matrix(rbind(c(test_1$statistic,test_1$parameter,test_1$p.value),
                       c(test_2$statistic,test_2$parameter,test_2$p.value)))
colnames(Tabla)<-c("Estadístico","Grados de libertad","Valor p")
rownames(Tabla)<-c("Error","Tiempo de ejecuci\u{F3}n")
table_latex<-xtable(Tabla)
digits(table_latex)<-matrix(c(0,0,2,2,0,0,5,5),nrow = 2,ncol=)
print(table_latex)