###########################################################
## Ejemplos de procesos estacionarios #####################

require('ggpubr')
require('scales')
require('psd')

###########################################################
# funcion autocorrelacion

autocov.1 = function(xt){
  lon = length(xt)
  
  acv = rep(0,lon)
  
  for(tau in 1:lon-1){
    tmp = 0
    for(t in 1:(lon-tau)){
      tmp = tmp + xt[t]*xt[t+tau]
    }
    acv[tau+1] = tmp/(lon-tau)
  }
  return(acv)
}
autocov.2 = function(xt){
  lon = length(xt)
  
  acv = rep(0,lon)
  
  for(tau in 1:lon-1){
    tmp = 0
    for(t in 1:(lon-tau)){
      tmp = tmp + xt[t]*xt[t+tau]
    }
    acv[tau+1] = tmp/lon
  }
  return(acv)
}


###########################################################
# Proceso tipo Alfa

set.seed(2018)

tmp = seq(-1,1,by=1/500)
lon = length(tmp)

lon2 = 100000
lon3 = 2*lon2+1

eps = rnorm( lon+ lon3 )

tm2 = ((1:lon3)-lon2-1)/(500)

vnt = (( sin(tm2*2*pi*12) - sin(tm2*2*pi*7) )/(tm2*2*pi))

X.t = rep(0,lon)

for(i in 1:lon){
  X.t[i] = sum(vnt*eps[1:lon3+i],na.rm = TRUE)
}

#plot(X.t,type='l')

m = rbind(X.t,tmp)

m = as.data.frame(t(m))
colnames(m) = c('proceso','tiempo')

ALFA = ggplot(m,aes(x=tiempo,y=proceso)) +
  xlab(NULL)+ylab(NULL)+
  theme_bw() +
  theme(panel.background = element_rect(fill = 'transparent',colour = NA),
        plot.background = element_rect(fill = 'transparent',colour = NA))+
  scale_x_continuous(expand=c(0,0))+
  #theme(axis.text.x = element_blank())+
  geom_line()

# funcion de autocovarianza

tmp2 = seq(0,2,by=1/500)
lon2 = length(tmp2)

R.real = ( sin(10*tmp2*2*pi)/(tmp2*2*pi) - sin(7*tmp2)/(tmp2*2*pi) )*100
R.real[1] = var(X.t)
R.suav = R.real*(1-tmp2/2)

R.norm = autocov.1(X.t)
R.star = autocov.2(X.t)

m1 = cbind( rbind(tmp2,R.real,rep(0,lon2))
            #rbind(tmp2,R.norm,rep(1,lon2))
            )
m2 = cbind( rbind(tmp2,R.suav,rep(0,lon2)),
            rbind(tmp2,R.star,rep(1,lon2)))

m1 = as.data.frame(t(m1))
colnames(m1) = c('tiempo','acv','indice')
m1$indice = factor(m1$indice,
                   labels=c('Real'))

ALFA.ACV = ggplot(m1,aes(x=tiempo,y=acv,linetype=indice)) +
  xlab(NULL) + ylab(NULL) +
  theme_bw() +
  theme(panel.background = element_rect(fill = 'transparent',colour = NA),
        plot.background = element_rect(fill = 'transparent',colour = NA))+
  scale_x_continuous(expand=c(0,0))+
  #theme(axis.text.x = element_blank())+
  theme(legend.position = 'none')+
  geom_line()

# periodograma
#periodogram(X.t)
#a = abs(fft(R.real))
#plot(tmp2,a,type='l')

###########################################################

# # graficos de los procesos
# ggarrange(OSCI,BLANCO,MA,ALFA,ncol=1,nrow=4,align = 'v',
#           labels='AUTO')
# ggsave('ruidos_ejemplos.pdf',device='pdf',dpi=600,
#        width = 15,height = 16,unit='cm')
# 
# # graficos de la autocovarianza
# ggarrange(OSCI.ACV,BLANCO.ACV,MA.ACV,ALFA.ACV,
#           ncol=1,nrow=4,align = 'v',labels='AUTO')
# ggsave('ruidos_ejemplos_autocovarianza.pdf',device='pdf',dpi=600,
#        width = 15,height = 16,unit='cm')
# 
# # graficos de la autocovarianza (R sesgado)
# ggarrange(OSCI.ACV.STR,BLANCO.ACV.STR,MA.ACV.STR,ALFA.ACV.STR,
#           ncol=1,nrow=4,align = 'v',labels='AUTO')
# ggsave('ruidos_ejemplos_autocovarianza_sesgado.pdf',device='pdf',dpi=600,
#        width = 15,height = 16,unit='cm')
