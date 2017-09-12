library(ggplot2)
library(gridExtra)
library(xtable)
library(bbmle)
setwd("C:/Users/ASUS/Desktop/Asignaturas/Computo paralelo/p5")
casos<-read.csv("casos_zika.csv",sep=",",header = T)
casos_hist<-casos[1:34,]
fuera_muestra<-casos[35:52,]
colnames(casos_hist)<-c("sem","caso")
colnames(fuera_muestra)<-c("sem","casof")
png("hist_zika.png", width=600, height=300, units="px")
plot(casos_hits$sem, casos_hist$caso, xlab="Semana", ylab="Casos nuevos", xlim=c(1,34))
lines(casos_hist$sem, casos_hist$caso, type="l")
graphics.off()
serie<-ts(casos_hist$caso,frequency = 52,start = c(2016,1,1))
plot(acf(serie))
fit0 <- mle2(casos_hist$caso~dpois(lambda=cmean),
             start=list(cmean=mean(casos_hist$caso)),data=casos)
days <- 18
changes <- rpois(days,lambda=coef(fit0))
plot(changes,type='l',ylab="Casos pronosticados",xlab="semana",
     main="Casos pronosticados bajo asunci\u{F3}n de un proceso Poisson")
plot(fuera_muestra,type='l',ylab="Casos pronosticados",xlab="semana",
     main="Casos reales")