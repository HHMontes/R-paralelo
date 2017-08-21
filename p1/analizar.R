#----------------FUNCIONES CONSTRUIDAS PARA EL ANALISIS DE DATOS------------------------
tabla <- read.csv("datos.csv",header = T)#Cargo simulaciones al ambiente de trabajo
setwd("C:/Users/ASUS/Desktop/Asignaturas/Computo paralelo")
repeticiones <-c(100,250,500,1000,2000)
longitudes <- c(10,25,50,100,200)
header <- matrix(c("Long","Dim","Rep","Conteo","Conteo_std"),ncol=5,nrow=1)

#ciclo que recorre los diferentes tamaños de muestra y separa datos en archivos
#independientes (sin importar longitudes de caminata ni dimension)

for (k in 1:length(repeticiones))#k=5
{ 
  library(lattice)#Librería para generar histogramas multiples
  library(dplyr)#Librería para modificar atributos de tablas
  tabla_rec<-tabla[(tabla[,3]==paste("No_Rep_",repeticiones[k])),]
  write.table(x=header,file = paste("tabla_",repeticiones[k],".csv"),sep=",",
              col.names=F,row.names = F,append=T)#Pongo encabezado
  write.table(x=tabla_rec,file=paste("tabla_",repeticiones[k],".csv"),sep=",",
              col.names=F,row.names = F,append=T)#Creo el archivo como tal
  
  #Cargo la tabla para un tamaño dado al ambiente de trabajo y convierto variables
  #en factores
  
  tabla_anova <- read.csv(paste("tabla_",repeticiones[k],".csv"),header = T)
  tabla_anova=mutate(tabla_anova,Dim = factor(Dim, levels=unique(Dim)))
  tabla_anova=mutate(tabla_anova,Long = factor(Long, levels=unique(Long)))
  
  #Gráfico un histograma de la distribución de conteos (absolutos y normalizados)
  #tanto para cada dimensión (sin considerar longitudes) como para cada longitud
  #(sin considerar dimensiones)
  
  #Histogramas para las dimensiones
  
  #Para conteos absolutos
  png(paste("histRetvsDim_Tam_",repeticiones[k],".png"))
  histogram(~ Conteo | Dim, 
             data=tabla_anova,
             layout=c(1,8),xlab="Retornos or\u{ED}gen",
            ylab="Frecuencia por dimensi\u{F3}n")
  dev.off()
  #Para conteos normalizados
  png(paste("histstdRetvsDim_Tam_",repeticiones[k],".png"))
  histogram(~ Conteo_std | Dim, 
            data=tabla_anova,
            layout=c(1,8),xlab="% Retornos or\u{ED}gen",
            ylab="Frecuencia por dimensi\u{F3}n")
  dev.off()
  
  #Histogramas para las longitudes 
  
  #Para conteos absolutos
  png(paste("histRetvslong_Tam",repeticiones[k],".png"))
  histogram(~ Conteo | Long, 
            data=tabla_anova,
            layout=c(1,5),xlab="Retornos or\u{ED}gen",
            ylab="Frecuencia por longitud")
  dev.off()
  #Para conteos normalizados
  png(paste("histstdRetvsLong_Tam_",repeticiones[k],".png"))
  histogram(~ Conteo_std | Long, 
             data=tabla_anova,
             layout=c(1,5),xlab="% Retornos or\u{ED}gen",
             ylab="Frecuencia por longitud")
  dev.off()
  
  #Ciclo que toma cada archivo de un tamaño de muestra dado y lo separa en archivos
  #independientes por cada dimensión (sin importar la longitud de la caminata)
  #Para cada dimension, creo, cargo y grafico un boxplot de la distribucion de
  #de conteos
  
  for (m in 1:8)
  { 
    #Creo conteos absolutos
    write.table(x=t(tabla_rec[(tabla_rec[,2]==m),4]),file=paste("Conteos_vs_dim_rep_",
                                                                repeticiones[k],".csv"),
                sep = ",",row.names = F, col.names=F, append = T)
    #Cargo conteos absolutos
    Conteos_vs_dim<-read.csv(file=paste("Conteos_vs_dim_rep_",
                                        repeticiones[k],".csv"),sep=",",header = F)
    #Creo conteos normalizados
    write.table(x=t(tabla_rec[(tabla_rec[,2]==m),5]),file=paste("Conteos_std_vs_dim_rep_",
                                                                repeticiones[k],".csv"),
                sep = ",",row.names = F, col.names=F, append = T)
    #Cargo conteos normalizados
    Conteos_std_vs_dim<-read.csv(paste("Conteos_std_vs_dim_rep_",
                                       repeticiones[k],".csv"),sep=",",header = F)
  }
  #Grafico el boxplot conteos absolutos
  png(paste("BoxRetvsDim_Tam_",repeticiones[k],".png"))
  boxplot(data.matrix(Conteos_vs_dim), use.cols=FALSE, 
          xlab="Dimensi\u{F3}n", ylab="Retorno al or\u{ED}gen", 
          main=paste("Retornos or\u{ED}gen vs Dimensión. \nTamaño de muestra: ",
                     repeticiones[k]))
  dev.off()
  #Grafico el boxplot conteos normalizados
  png(paste("BoxStdRetvsDim_Tam_",repeticiones[k],".png"))
  boxplot(data.matrix(Conteos_std_vs_dim), use.cols=FALSE, 
          xlab="Dimensi\u{F3}n", ylab="% Retorno al or\u{ED}gen", 
          main=paste("% Retornos or\u{ED}gen vs Dimensión. \nTamaño de muestra: ",
                     repeticiones[k]))
  dev.off()
  
  #Ciclo que toma cada archivo de un tamaño de muestra dado y lo separa en archivos
  #independientes por cada longitud (sin importar la dimensión de la caminata)
  
  for (l in 1:5)
  {
    #Creo conteos absolutos
    write.table(x=t(tabla_rec[(tabla_rec[,1]==longitudes[l]),4]),
                file=paste("Conteos_vs_long_rep_",repeticiones[k],".csv"),
                sep = ",",row.names = F, col.names=F, append = T)
    #Cargo conteos absolutos
    Conteos_vs_long<-read.csv(file=paste("Conteos_vs_long_rep_",
                                         repeticiones[k],".csv"),sep=",",header = F)
        #Creo conteos normalizados
    write.table(x=t(tabla_rec[(tabla_rec[,1]==longitudes[l]),5]),
                file=paste("Conteos_std_vs_long_rep_",repeticiones[k],".csv"),
                sep = ",",row.names = F, col.names=F, append = T)
    #Cargo conteos normalizados
    Conteos_std_vs_long<-read.csv(paste("Conteos_std_vs_long_rep_",
                                        repeticiones[k],".csv"),sep=",",header = F)
  }
  #Grafico el boxplot conteos absolutos
  png(paste("BoxRetvslong_Tam_",repeticiones[k],".png"))
  boxplot(data.matrix(Conteos_vs_long), use.cols=FALSE, 
          xlab="Longitud", ylab="Retorno al or\u{ED}gen", 
          main=paste("Retornos or\u{ED}gen vs Longitud. \nTamaño de muestra: ",
                     repeticiones[k]))
  dev.off()
  #Creo boxplot conteos normalizados
  png(paste("BoxStdRetvslong_Tam_",repeticiones[k],".png"))
  boxplot(data.matrix(Conteos_std_vs_long), use.cols=FALSE, 
          xlab="Longitud", ylab="% Retorno al or\u{ED}gen", 
          main=paste("% Retornos or\u{ED}gen vs Longitud. \nTamaño de muestra: ",
                     repeticiones[k]))
  dev.off()
  
}