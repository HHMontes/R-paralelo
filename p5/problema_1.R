library(ggplot2)
library(gridExtra)
library(xtable)
setwd("C:/Users/ASUS/Desktop/Asignaturas/Computo paralelo/p5")
f <- function(x) { return(1 / (exp(x) + exp(-x))) }
suppressMessages(library(distr))
g <- function(x) { return((2 / pi) * f(x)) }
generador  <- r(AbscontDistribution(d = g)) # creamos un generador
#tam<-c(5000,50000,500000,5000000)
tam<-c(500,1000,1500,2000)
desde <- 3
hasta <- 7; #Intervalo de integraci\u{F3}n
parte <- function(desde,hasta,pedazo){
  valores <- generador(pedazo)
  return(suma=sum(valores >= desde & valores <= hasta))
}#Funci\u{F3}n que calcula la cantidad de n\u{FA}ros en el intervalo
resultados<-data.frame(Tamano=factor(),
                       Integral=double(),
                       Tiempos=double())
tiempos<-vector()
integrales<-vector()
cuantos <- 500 #Distribuimos la generaci\u{F3}n de n\u{FA}meros en 500 pedazos
cluster<-makeCluster(detectCores(logical = F))
clusterExport(cluster,"desde")
clusterExport(cluster,"hasta")
clusterExport(cluster,"parte")
clusterExport(cluster,"generador")
for (t in tam)
{ clusterExport(cluster,"t")
  for (i in 1:50)
  {
    clusterExport(cluster,"i")
    t0<-Sys.time()
    integrales[i]<-sum(parSapply(
      cluster,1:cuantos,function(x) {parte(desde,hasta,generador(t))}))*(pi/(2*t*cuantos))
    tf<-Sys.time()
    tiempos[i]=(tf-t0)
  }
  resultados<-rbind(resultados,cbind(Tamano=t,Integral=integrales,Tiempo=tiempos))
}
stopCluster(cluster)

Valor_referencia<-atan(exp(7))-atan(exp(3))
png("aproximaciones y errores.png",width = 617, height = 354)
plot1<-ggplot(resultados, aes(x=as.factor(Tamano),
                       y=Integral,
                       fill=as.factor(Tamano)),xlab(Tamano)) + 
  geom_boxplot(alpha=0.3)+theme(legend.position="none")+
  geom_hline(yintercept = Valor_referencia)+
  labs(x = "Tama\u{F1}o de muestra",y = "Valores aproximados")
plot2<-ggplot(resultados, aes(x=as.factor(Tamano),
                       y=abs(Integral-Valor_referencia)/Valor_referencia,
                       fill=as.factor(Tamano)),xlab(Tamano)) + 
  geom_boxplot(alpha=0.3)+theme(legend.position="none")+
  labs(x = "Tama\u{F1}o de muestra",y = "Error relativo")
grid.arrange(plot1, plot2, nrow=1, ncol=2)
graphics.off()
png("tiempos.png")
ggplot(resultados, aes(x=as.factor(Tamano),
                       y=Tiempo,
                       fill=as.factor(Tamano)),xlab(Tamano)) + 
  geom_boxplot(alpha=0.3)+theme(legend.position="none")+
  labs(x = "Tama\u{F1}o de muestra",y = "Tiempos de ejecuci\u{F3}n")
graphics.off()

test_1<-kruskal.test(abs(resultados$Integral-Valor_referencia)/Valor_referencia~as.factor(resultados$Tamano))
test_2<-kruskal.test(resultados$Tiempo~as.factor(resultados$Tamano))
Tabla<-as.matrix(rbind(c(test_1$statistic,test_1$parameter,test_1$p.value),
                       c(test_2$statistic,test_2$parameter,test_2$p.value)))
colnames(Tabla)<-c("Estadístico","Grados de libertad","Valor p")
rownames(Tabla)<-c("Error","Tiempo de ejecuci\u{F3}n")
table_latex<-xtable(Tabla)
digits(table_latex)<-matrix(c(0,0,2,2,0,0,5,5),nrow = 2,ncol=)
print(table_latex)
#write.table(resultados,file="resultados_50000_500000.csv",sep=",")
