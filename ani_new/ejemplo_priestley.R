library(tuneR)
library(fractal)

set.seed(2016)
serie = noise(kind = c("power"),samp.rate=512,duration=512*6,alpha=1)
datos = serie@left

tt = 1:(512*6)
tt = tt/512

###################################################################################
###################################################################################

s = stationarity(datos)
espec = s$anova
frecu = s$freq

le = 279

dev.off()
par(mfrow=c(2,1),mar=c(1,2,1,1)+0.1)

plot(tt,datos,xlab='',ylab='',col='white')

q = 0
abline(v=(le*q+le/2)/512)

for(k in 0:10){
  lines(tt[(le*k+1):(le*(k+1))],datos[(le*k+1):(le*(k+1))],xlab='',ylab='',col=rainbow(11)[k+1])
}

plot(frecu,espec[q+1,1:length(frecu)],type='l',xlab='',ylab='',col=rainbow(11)[q+1])

###################################################################################
###################################################################################

library(animation)

s = stationarity(datos)
espec = s$anova
frecu = s$freq

s
le = 279

saveGIF({
  ani.options(interval = 1, nmax = 50)

  for(q in 0:10){
    par(mfrow=c(2,1),mar=c(2,2,1,1)+0.1)
    
    plot(tt,datos,xlab='',ylab='',col='white')
    
    abline(v=(le*q+le/2)/512)
    for(k in 0:10){
      lines(tt[(le*k+1):(le*(k+1))],datos[(le*k+1):(le*(k+1))],xlab='',ylab='',col=rainbow(11)[k+1])
    }
    plot(frecu,espec[q+1,1:23],type='l',xlab='',ylab='',col=rainbow(11)[q+1],ylim=c(-6,1),lwd=5)
    
    ani.pause()
  }
  
}, movie.name="espectro_demo_0.gif", ani.width=623, ani.height=400)

