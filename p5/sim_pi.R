library(ggplot2)
runs <- 10000
#runif samples from a uniform distribution
xs <- runif(runs,min=-0.5,max=0.5)
ys <- runif(runs,min=-0.5,max=0.5)
datos<-as.data.frame(cbind(x=xs,y=ys))
in.circle <- xs^2 + ys^2 <= 0.5^2
mc.pi <- (sum(in.circle)/runs)*4
png("Grafico_pi.png",width = 400,height = 400)
ggplot(datos,aes(x,y),pch='.',asp=1,
       xlim=c(-0.5,0.5),ylim=c(-0.5,0.5))+
  geom_point(aes(col=ifelse(in.circle,"blue","grey")))+
  xlab("Coordenada x")+ylab("Coordenada y")+
  ggtitle (paste("Approximaci\u{F3}n de Pi =",mc.pi))+
  theme(plot.title = element_text(size=rel(1.3),
                                  face="bold", color="Black",
                                  lineheight=1.5),legend.justification = "top",
        legend.position="none")
graphics.off()
