###########################################################
## Ejemplos de escalamiento  ##############################

#dir_datos = 'C:/Users/EQUIPO 1/Desktop/julio/tesis_respaldo/ejemplos_viejos'
dir_graf  = 'C:/Users/EQUIPO 1/Desktop/julio/tesis_respaldo/TESIS/scripts_graf_res'

require('ggpubr')
require('locits')
require('fractal')
require('hms')
require('scales')

require('reshape2')

###########################################################
# proceso MA cambiante, anchi de banda finito

phi = function(t){
  return((tanh(t)+1)/2)
}

frec = 1000

set.seed(2018)

mini = -3.5
maxi =  3.5

tmp  = seq(mini,maxi,by=1/frec)
lon  = length(tmp)

X.t = rep(0,lon)

lon2 = ceiling(phi(mini)/2*frec) + ceiling(phi(maxi)/2*frec) + 1
t.0  = ceiling(phi(mini)/2*frec)

eps = rnorm( lon + lon2 )

for( i in 1:lon ){
  t = tmp[i]
  
  t.ini = t.0 + i - ceiling(phi(t)/2*frec)
  t.fin = t.0 + i + ceiling(phi(t)/2*frec)
  
  X.t[i] = sum(eps[t.ini:t.fin])/sqrt(t.fin-t.ini+1)
}

m = rbind(X.t,tmp)

m = as.data.frame(t(m))
colnames(m) = c('proceso','tiempo')

m$tiempo = (m$tiempo+3.5)*frec

MACH1 = ggplot(m,aes(x=tiempo,y=proceso)) +
  xlab('t')+ylab('Z(t)')+
  theme_bw() +
  theme(panel.background = element_rect(fill = 'transparent',colour = NA),
        plot.background = element_rect(fill = 'transparent',colour = NA))+
  scale_x_continuous(expand=c(0,0),
                     breaks = pretty_breaks(n=8))+
  #theme(axis.text.x = element_blank())+
  annotate('rect',xmin=   0,xmax=1000,ymin=-Inf,ymax=Inf,
           color='gray80',alpha=.2) +
  annotate('rect',xmin=2000,xmax=3000,ymin=-Inf,ymax=Inf,
           color='gray80',alpha=.2) +
  annotate('rect',xmin=4000,xmax=5000,ymin=-Inf,ymax=Inf,
           color='gray80',alpha=.2) +
  annotate('rect',xmin=6000,xmax=7000,ymin=-Inf,ymax=Inf,
           color='gray80',alpha=.2) +
  rotate_x_text(angle = 45) +
  geom_line()

ggsave('proceso_Z.pdf',device='pdf',dpi=600,
       width = 15,height = 8,unit='cm', 
       path=dir_graf)


#################################################################
#################################################################

uu = (1:501 - 251)/250

gg = 3/((pi*(uu))**2)*( (sin(pi*uu)/(pi*uu)) - cos(pi*uu) )
gg[is.na(gg)]=1

ig = sum(gg**2)

gg = gg/ig

UU = matrix(NA,ncol=length(X.t)+10,nrow=250)

for(t0 in 1:(length(X.t)-500) -1 +250 ){
  print(100*(t0+2-250)/(length(X.t)-500))
  
  tmp = X.t[t0 + 1:501]*gg
  
  fou = abs(fft(tmp))**2
  fou = fou[1:250]
  
  UU[,t0+250] = fou
}

UU = as.data.frame(t(UU))
colnames(UU) = 1:250
UU$tiempo = 1:length(UU[,1])-1

require('reshape2')

UU2 = melt(UU,id.vars = 'tiempo')
colnames(UU2) = c('tiempo','frecuencia','U')

UU2$frecuencia = as.numeric(as.character(UU2$frecuencia))

Uplot = ggplot(UU2,aes(x=tiempo,y=frecuencia,fill=log(U))) +
  theme_bw()+
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_distiller(palette = 'Spectral')+
  theme(legend.position = 'bottom') +
  xlab('t') + ylab(expression(omega)) +
  labs(fill=expression(paste('log( |',
                             U(t,omega),'|',phantom(.)^2,
                             ')' )))+ 
  scale_x_continuous(expand=c(0,0),
                     breaks = pretty_breaks(n=8))+
  rotate_x_text(angle = 45) +
  geom_raster()
plot(Uplot)


ggsave('proceso_Z_logU.png',device='png',dpi=600,
       width = 15,height = 8,unit='cm', 
       path=dir_graf)

#################################################################
#################################################################

uu = (1:501 - 251)/250

ww = 3/((pi*(uu))**2)*( (sin(pi*uu)/(pi*uu)) - cos(pi*uu) )
ww[is.na(gg)]=1

hh = matrix(NA,ncol=length(X.t)+10,nrow=250)

UU[is.na(UU)] = 0

for(t0 in 1:(length(X.t)-750) -2+500 ){
  print(100*(t0+2-250)/(length(X.t)-500))
  for(om in  1:(length(UU[1,])-1)){
    #print(t0)
    #print(om)
    #print(NULL)
    
    tmp = sum(UU[t0 + 1:501,om]*gg)
    
    #hh[,t0+(1:length(tmp))-1] = tmp
    hh[om,t0] = tmp
  }
}

hh = as.data.frame(t(hh))
colnames(hh) = 1:250
hh$tiempo = 1:length(hh[,1])-1

require('reshape2')

hh2 = melt(hh,id.vars = 'tiempo')
colnames(hh2) = c('tiempo','frecuencia','U')

hh2$frecuencia = as.numeric(as.character(hh2$frecuencia))

hplot = ggplot(hh2,aes(x=tiempo,y=frecuencia,fill=log(U))) +
  theme_bw()+
  rotate_x_text(angle = 45) +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_distiller(palette = 'Spectral')+
  theme(legend.position = 'bottom') +
  xlab('t') + ylab(expression(omega)) +
  labs(fill=expression(paste('log( ',h(t,omega),' )' )))+ 
  scale_x_continuous(expand=c(0,0),
                     breaks = pretty_breaks(n=8))+
  rotate_x_text(angle = 45) +
  geom_raster()

plot(hplot)

ggsave('proceso_Z_logh.png',device='png',dpi=600,
       width = 15,height = 8,unit='cm', 
       path=dir_graf)

##################################################################
##################################################################

z   = stationarity(X.t,n.block=14)
print(z)

w = as.data.frame(z$anova)
w = w[1:(length(w[,1])-1),1:(length(w[1,])-1)]

w$bloque = 1:(length(w[,1]))

colnames(w) = c(z$freq,'bloque')

w2 = melt(w,id.vars='bloque')
colnames(w2) = c('bloque','frec','Y')

w2$frec = as.numeric(as.character(w2$frec))*500

A = ggplot(w2,aes(x=bloque,y=frec,fill=Y-5)) +
  theme_bw()+
  rotate_x_text(angle = 45) +
  scale_x_continuous(expand = c(0,0),breaks = 1:14) +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_distiller(palette = 'Spectral')+
  theme(legend.position = 'bottom') +
  xlab('Número de bloque') + ylab(expression(omega)) +
  labs(fill=expression(Y(t,omega)))+ 
  geom_raster()
plot(A)

ggarrange(MACH1,Uplot,hplot,A,ncol=1,nrow=4,align = 'v',
          heights = c(.19,.27,.27,.27),labels='AUTO')

ggsave('proceso_Z_varios.png',device='png',dpi=600,
       width = 15,height = 17,unit='cm', 
       path=dir_graf,scale=1.4)



ggarrange(MACH1,hplot,A,ncol=1,nrow=3,align = 'v',
          heights = c(.3,.4,.4),labels='AUTO')

ggsave('proceso_Z_varios_v2.png',device='png',dpi=600,
       width = 15,height = 17,unit='cm', 
       path=dir_graf,scale=1)

##################################################################
##################################################################

m$indice = c(rep(1:1000,7),1001)

m$bloque = ceiling(m$tiempo/1000)
m$bloque[1] = 1

m$bloque = factor(m$bloque,
                  labels=c('Bloque 1','Bloque 2','Bloque 3','Bloque 4',
                           'Bloque 5','Bloque 6','Bloque 7'))

NN = ggplot(m,aes(x=indice-1,y=proceso)) +
  xlab('Indice i')+ylab(expression(Z(t[i])))+
  theme_bw() +
  facet_grid(bloque~.) +
  theme(panel.background = element_rect(fill = 'transparent',colour = NA),
        plot.background = element_rect(fill = 'transparent',colour = NA))+
  scale_x_continuous(expand=c(0,0),
                     breaks = pretty_breaks(n=8))+
  #theme(axis.text.x = element_blank())+
  rotate_x_text(angle = 45) +
  geom_line()
plot(NN)

ggsave('proceso_Z_fragmentado.pdf',device='pdf',dpi=600,
       width = 15,height = 16,unit='cm', 
       path=dir_graf)

##################################################################
##################################################################

aa = hh[c( 500,1500,2500,3500,4500,5500,6500),]

aa$tiempo = factor(aa$tiempo,
                   labels=c('Bloque 1','Bloque 2','Bloque 3','Bloque 4',
                            'Bloque 5','Bloque 6','Bloque 7'))

bb = melt(aa,id.vars = 'tiempo')
colnames(bb) = c('tiempo','frec','hh')

bb$frec = as.numeric(as.character(bb$frec))
bb2 = bb[is.element(bb$frec,c(1,50,100,150,200,250)),]

cc = dcast(bb2,tiempo~frec)
cc[,2:7] = log(cc[,2:7])

MM = ggplot(bb,aes(x=frec,y=log(hh))) +
  xlab(expression(omega))+
  ylab(expression(Y(t[i],omega)))+
  theme_bw() +
  facet_grid(tiempo~.) +
  theme(panel.background = element_rect(fill = 'transparent',colour = NA),
        plot.background = element_rect(fill = 'transparent',colour = NA))+
  #scale_x_continuous(expand=c(0,0),
  #                   breaks = pretty_breaks(n=8))+
  #theme(axis.text.x = element_blank())+
  expand_limits(x=c(0,250))+
  rotate_x_text(angle = 45) +
  geom_line(color='gray') +
  geom_point(data=bb2,aes(x=frec,y=log(hh)))
plot(MM)

ggsave('proceso_Z_Y_fragmentado.pdf',device='pdf',dpi=600,
       width = 15,height = 16,unit='cm', 
       path=dir_graf)

ggarrange(NN,MM,ncol=2,nrow=1,align='h',labels='AUTO')

ggsave('proceso_Z_fragmentado_doble.pdf',device='pdf',dpi=600,
       width = 15,height = 16,unit='cm', 
       path=dir_graf)
