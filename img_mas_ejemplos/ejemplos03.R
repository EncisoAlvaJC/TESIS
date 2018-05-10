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
# Proceso oscilante

set.seed(2018)

tmp = seq(-1,1,by=1/500)
lon = length(tmp)

m   = matrix(ncol=3,nrow=0)

for(i in 1:3){ 
  phi = runif(1,0,2*pi)
  
  X.t = sin(pi*tmp+phi)
  
  m = rbind( m, t(rbind(tmp,X.t,rep(i,lon))) )
} 

m = as.data.frame(m)
colnames(m) = c('tiempo','proceso','indice')
m$indice    = factor(m$indice)

OSCI = ggplot(m,aes(x=tiempo,y=proceso,linetype=indice)) +
  xlab(NULL) + ylab('OSCI') +
  theme_bw() +
  theme(axis.title = element_text(colour = 'white'))+
  theme(panel.background = element_rect(fill = 'transparent',colour = NA),
        plot.background = element_rect(fill = 'transparent',colour = NA))+
  scale_x_continuous(expand=c(0,0))+
  theme(axis.text.x = element_blank())+
  theme(legend.position = 'none')+
  geom_line()

###########################################################
# Proceso Ruido Blanco

set.seed(2018)

tmp = seq(-1,1,by=1/500)
lon = length(tmp)

X.t = rnorm(lon)

m = rbind(X.t,tmp)

m = as.data.frame(t(m))
colnames(m) = c('proceso','tiempo')

BLANCO = ggplot(m,aes(x=tiempo,y=proceso)) +
  xlab(NULL) + ylab(NULL) +
  theme_bw() +
  theme(panel.background = element_rect(fill = 'transparent',colour = NA),
        plot.background = element_rect(fill = 'transparent',colour = NA))+
  scale_x_continuous(expand=c(0,0))+
  theme(axis.text.x = element_blank())+
  geom_line()

###########################################################
# Proceso de Wiener

set.seed(2018)

tmp = seq(-1,1,by=1/500)
lon = length(tmp)

X.t = rnorm(lon)
X.t[1] = 0

for(i in 2:length(X.t)){
  X.t[i] = X.t[i]*sqrt(1/500) + X.t[i-1]
}

m = rbind(X.t,tmp)

m = as.data.frame(t(m))
colnames(m) = c('proceso','tiempo')

WIENER = ggplot(m,aes(x=tiempo,y=proceso)) +
  xlab(NULL) + ylab(NULL) +
  theme_bw() +
  theme(panel.background = element_rect(fill = 'transparent',colour = NA),
        plot.background = element_rect(fill = 'transparent',colour = NA))+
  scale_x_continuous(expand=c(0,0))+
  theme(axis.text.x = element_blank())+
  geom_line()

###########################################################
# Proceso Medias Moviles (MA)

set.seed(201805)

tmp  = seq(-1,1,by=1/500)
lon  = length(tmp)

lon2 = 25
lon3 = 2*lon2+1

eps = rnorm( lon+ lon3 )
vnt = rep( 1/sqrt(lon3), lon3 )
X.t = rep(0,lon)

for(i in 1:lon){
  X.t[i] = sum( vnt*eps[1:lon3 +i ])
}

m = rbind(X.t,tmp)

m = as.data.frame(t(m))
colnames(m) = c('proceso','tiempo')

MA = ggplot(m,aes(x=tiempo,y=proceso)) +
  xlab(NULL)+ylab(NULL)+
  theme_bw() +
  theme(panel.background = element_rect(fill = 'transparent',colour = NA),
        plot.background = element_rect(fill = 'transparent',colour = NA))+
  scale_x_continuous(expand=c(0,0))+
  #theme(axis.text.x = element_blank())+
  geom_line()

###########################################################

# graficos de los procesos
ggarrange(OSCI,BLANCO,WIENER,MA,ncol=1,nrow=4,align = 'v',
          labels='AUTO')
ggsave('ruidos_ejemplos.pdf',device='pdf',dpi=600,
       width = 15,height = 16,unit='cm')
