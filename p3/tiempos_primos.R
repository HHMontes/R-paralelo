setwd("C:/Users/ASUS/Desktop/Asignaturas/Computo paralelo/p3")
suppressMessages(library(doParallel))
max_nucleos<-(detectCores(logical=FALSE))
#Conteo de números según tipo.
etiquetas <- function(n) {
  if (n == 1 || n == 2) {
    return("Trivial")
  }
  if (n %% 2 == 0) {
    return("Par")
  }
  for (i in seq(3, max(3, ceiling(sqrt(n))), 2)) {
    if ((n %% i) == 0) {
      return("Impar no primo")
    }
  }
  return("Impar primo")
}
desde <- 10000
hasta <-  30000
original <- desde:hasta
registerDoParallel(makeCluster(max_nucleos))
o_num <- foreach(n = original, .combine=c) %dopar% etiquetas(n)
stopImplicitCluster()
table(o_num)

#Ahora ordenamos los elementos del vector tanto en dificultad
#creciente como en decreciente

dif_crec_t<-numeric()
dif_dec_t<-numeric()
tipos<-c("Trivial","Par","Impar no primo","Impar primo")
k<-1
for (i in tipos)
{
  for (j in desde:hasta)
  { 
    if (o_num[j-desde+1]==i)
    {
      dif_crec_t[k]<-j
      k<-k+1
    }
  }
}
dif_dec_t<-rev(dif_crec_t)

#Finalmente evaluamos los tiempos de ejecución para los diferentes
#órdenes y considerando diversa cantidad de núcleos.

nucleos<-seq(2:(detectCores(logical=FALSE)))
primo <- function(n) {
  if (n == 1 || n == 2) {
    return(TRUE)
  }
  if (n %% 2 == 0) {
    return(FALSE)
  }
  for (i in seq(3, max(3, ceiling(sqrt(n))), 2)) {
    if ((i<n) && (n %% i) == 0) {
      return(FALSE)
    }
  }
  return(TRUE)
}
invertido <- hasta:desde
replicas <- 50
m_ot <-  matrix(nrow = length(nucleos),ncol=replicas)
m_it <-  matrix(nrow = length(nucleos),ncol=replicas)
m_at <-  matrix(nrow = length(nucleos),ncol=replicas)
m_dif_crec_t <-  matrix(nrow = length(nucleos),ncol=replicas)
m_dif_dec_t <-  matrix(nrow = length(nucleos),ncol=replicas)
names_var<-paste(nucleos," n\u{FA}cleos")
for (i in nucleos)
{ 
  registerDoParallel(makeCluster(i))
  ot<-numeric()
  it<-numeric()
  at<-numeric()
  df_c_t<-numeric()
  df_d_t<-numeric()
  for (r in 1:replicas) {
    ot<- c(ot, system.time(foreach(n = original, .combine=c) %dopar% primo(n))[3]) # de menor a mayor
    it <- c(it, system.time(foreach(n = invertido, .combine=c) %dopar% primo(n))[3]) # de mayor a menor
    at <- c(at, system.time(foreach(n = sample(original), .combine=c) %dopar% primo(n))[3]) # orden aleatorio
    df_c_t <- c(df_c_t, system.time(foreach(n = dif_crec_t, .combine=c) %dopar% primo(n))[3]) # orden menor a mayor dificultad
    df_d_t <- c(df_d_t, system.time(foreach(n = dif_dec_t, .combine=c) %dopar% primo(n))[3]) # orden mayor a menor dificultad
  }
  m_ot[i,]<-ot
  m_it[i,]<-it
  m_at[i,]<-at
  m_dif_crec_t[i,] <- df_c_t
  m_dif_dec_t[i,] <-  df_d_t
  stopImplicitCluster()
}
print(paste("Resultados de tiempos de ejecuci\u{F3}n para ",i," n\u{FA}cleos"))
print(apply(m_ot,1,summary))
print(apply(m_it,1,summary))
print(apply(m_at,1,summary))
print(apply(m_dif_crec_t,1,summary))
print(apply(m_dif_dec_t,1,summary))

write.table(t(m_ot),file="m_ot.csv",sep=",",row.names = F, col.names = names_var)
write.table(t(m_it),file="m_it.csv",sep=",",row.names = F, col.names = names_var)
write.table(t(m_at),file="m_at.csv",sep=",",row.names = F, col.names = names_var)
write.table(t(m_dif_crec_t),file="m_dif_crec_t.csv",sep=",",row.names = F, col.names = names_var)
write.table(t(m_dif_dec_t),file="m_dif_dec_t.csv",sep=",",row.names = F, col.names = names_var)
write.table(t(rbind(dif_crec_t,dif_dec_t)),file="Vectores_dif.csv",sep=",",row.names = F,append=T,col.names = c("Dificultad Creciente","Dificultad decreciente"))
