library(tuneR)
library(fractal)

set.seed(2016)
serie_full = noise(kind = c("power"),samp.rate=512,duration=512*7,alpha=1)
serie = serie_full[1:(512*6)]
datos = serie@left
datos_full = serie_full@left

tt = 1:(512*6)
tt = tt/512

###################################################################################
###################################################################################

le = 279
jump = 2500 #0:10:(512*6-279)
jum_step = 10
max_jum = (512*6-le)/jum_step

###################################################################################

dev.off()
par(mfrow=c(2,1),mar=c(1,2,1,1)+0.1)
plot(tt,datos,xlab='',ylab='',col='white')
lines(tt[jump:(jump+le)],datos[jump:(jump+le)],type='l',col=rainbow(max_jum)[jump/jum_step+1])
lines(tt[0:jump],datos[0:jump],type='l')
lines(tt[(jump+le):length(tt)],datos[(jump+le):length(tt)],type='l')


abline(v=(jump+le/2)/512)

s = stationarity(datos_full[jump:(jump+le)],n.block = 2)
espec = s$anova
frecu = s$freq

plot(frecu,espec[1,1:length(frecu)],type='l',xlab='',ylab='',col=rainbow(max_jum)[jump/jum_step+1])

###################################################################################
###################################################################################

library(animation)

saveGIF({
  ani.options(interval = 0.2, nmax = 50)
  
  set.seed(2016)
  serie_full = noise(kind = c("power"),samp.rate=512,duration=512*7,alpha=1)
  serie = serie[1:(512*6)]
  datos = serie@left
  datos_full = serie_full@left
  
  tt = 1:(512*6)
  tt = tt/512
  
  le = 277
  jum_step = 25
  max_jum = (512*6-le)/jum_step
  
  for(j in 0:(max_jum-1)){
    
    jump = j*jum_step
    
    par(mfrow=c(2,1),mar=c(1,2,1,1)+0.1)
    plot(tt,datos,xlab='',ylab='',col='white')
    lines(tt[jump:(jump+le)],datos[jump:(jump+le)],type='l',col=rainbow(max_jum)[jump/jum_step+1])
    lines(tt[0:jump],datos[0:jump],type='l')
    lines(tt[(jump+le):length(tt)],datos[(jump+le):length(tt)],type='l')
    
    
    abline(v=(jump+le/2)/512)
    
    s = stationarity(datos_full[jump:(jump+le)],n.block = 2)
    espec = s$anova
    frecu = s$freq
    
    plot(frecu,exp(espec[1,1:length(frecu)]),type='l',xlab='',ylab='',col=rainbow(max_jum)[jump/jum_step+1],ylim=c(0,1.5),lwd=5,pch=1)
    
    ani.pause()
  }
}, movie.name="priestley_spectra_full.gif", ani.width=623, ani.height=300)
#}, movie.name="priestley_spectra.gif", ani.width=623, ani.height=400)


###################################################################################
###################################################################################


saveGIF({
  ani.options(interval = 0.5, nmax = 50)
  
  set.seed(2016)
  serie_full = noise(kind = c("power"),samp.rate=512,duration=512*7,alpha=1)
  serie = serie[1:(512*6)]
  datos = serie@left
  datos_full = serie_full@left
  
  tt = 1:(512*6)
  tt = tt/512
  
  le = 279
  jum_step = 279
  max_jum = (512*6)/jum_step
  
  for(j in 0:(max_jum-1)){
    
    jump = j*jum_step
    
    par(mfrow=c(2,1),mar=c(1,2,1,1)+0.1)
    plot(tt,datos,xlab='',ylab='',col='white')
    lines(tt[jump:(jump+le)],datos[jump:(jump+le)],type='l',col=rainbow(max_jum)[jump/jum_step+1])
    lines(tt[0:jump],datos[0:jump],type='l')
    lines(tt[(jump+le):length(tt)],datos[(jump+le):length(tt)],type='l')
    
    
    abline(v=(jump+le/2)/512)
    
    s = stationarity(datos_full[jump:(jump+le)],n.block = 2)
    espec = s$anova
    frecu = s$freq
    
    plot(frecu,exp(espec[1,1:length(frecu)]),type='l',xlab='',ylab='',col=rainbow(max_jum)[jump/jum_step+1],ylim=c(0,1.5),lwd=5,pch=1)
    
    ani.pause()
  }
}, movie.name="priestley_spectra_small.gif", ani.width=623, ani.height=300)

###################################################################################
###################################################################################

z = stationarity(datos)

y = z$anova
f = z$freq

write.csv(y,'ej_priestley_y.csv')
write.csv(f,'ej_priestley_f.csv')
