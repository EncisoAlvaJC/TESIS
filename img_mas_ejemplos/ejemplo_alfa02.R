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

set.seed(201805)

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
  xlab('t')+ylab('X(t)')+
  theme_bw() +
  theme(panel.background = element_rect(fill = 'transparent',colour = NA),
        plot.background = element_rect(fill = 'transparent',colour = NA))+
  scale_x_continuous(expand=c(0,0))+
  #theme(axis.text.x = element_blank())+
  geom_line()

plot(ALFA)

# funcion de autocovarianza

tmp2 = seq(0,2,by=1/500)
lon2 = length(tmp2)

R.real = ( sin(12*tmp2*2*pi)/(2*pi*tmp2) - sin(7*tmp2)/(2*pi*tmp2) )
R.real[1] = 9

R.real = R.real*var(R.real)/9

m1 = cbind( rbind(tmp2,R.real,rep(0,lon2))
            #rbind(tmp2,R.norm,rep(1,lon2))
            )

m1 = as.data.frame(t(m1))
colnames(m1) = c('tiempo','acv','indice')
m1$indice = factor(m1$indice,
                   labels=c('Real'))

ALFA.ACV = 
  ggplot(m1,aes(x=tiempo,y=acv,linetype=indice)) +
  xlab(expression(tau)) + ylab(expression(R(tau))) +
  theme_bw() +
  theme(panel.background = element_rect(fill = 'transparent',colour = NA),
        plot.background = element_rect(fill = 'transparent',colour = NA))+
  scale_x_continuous(expand=c(0,0))+
  #theme(axis.text.x = element_blank())+
  theme(legend.position = 'none')+
  geom_hline(yintercept = 0,col='gray') +
  geom_line()
plot(ALFA.ACV)

tmp3 = seq(0,20,by=1/500)
lon3 = length(tmp3)

hh = rep(0,length(tmp3))
hh[tmp3>=7 & tmp3<=12] = 9

mm = cbind( rbind(tmp3,hh,rep(0,lon3))
            #rbind(tmp2,R.norm,rep(1,lon2))
)

mm = as.data.frame(t(mm))
colnames(mm) = c('tiempo','espectro','indice')
mm$indice = factor(mm$indice,
                   labels=c('Real'))

ALFA.HH = 
  ggplot(mm,aes(x=tiempo,y=espectro,linetype=indice)) +
  xlab(expression(omega)) + ylab(expression(h(omega))) +
  theme_bw() +
  theme(panel.background = element_rect(fill = 'transparent',colour = NA),
        plot.background = element_rect(fill = 'transparent',colour = NA))+
  scale_x_continuous(expand=c(0,0),
                     breaks=c(0,5,10,15,20,7,12))+
  #theme(axis.text.x = element_blank())+
  #scale_y_continuous(breaks = c(0,2.5,5,7.5,10)) +
  expand_limits(y=10) +
  theme(legend.position = 'none')+
  geom_hline(yintercept = 0,col='gray') +
  geom_line()
plot(ALFA.HH)

ggarrange(ALFA,ALFA.ACV,ALFA.HH,
          ncol=1,nrow=3,align = 'v',labels='AUTO')

ggsave('proceso_alfa.pdf',device='pdf',dpi=600,
       width = 15,height = 16,unit='cm')

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
