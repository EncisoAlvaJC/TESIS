###########################################################
## Ruidos de colores #####################################

require('ggpubr')
require('scales')
require('tuneR')
require('psd')

###########################################################

set.seed(2018)

le1 = 100000
le2 = 1000

ee = rnorm(le2 + 2*le1 + 1)

tt = (1:le1)
gg = sin(tt/12)/tt-sin(tt/7)/tt
#gg = sin(tt/30)/tt-sin(tt/12)/tt
gg = c(rev(gg),0,gg)

#gg = ((0.049922035 -0.095993537/(tt) +0.050612699/(tt**2) -0.004408786/(tt**3))/
#        (1 -2.494956002/(tt) +2.017265875/(tt**2) -0.522189400/(tt**3)))

le1 = length(gg)

pn = rep(0,le2)

for(i in 1:le2){
  pn[i] = sum(gg*ee[1:le1+ i-1])
}

s = pspectrum(pn,x.frqsamp = 500 ,Nyquist.normalize = F)
plot(pn,type='l')
plot(s$freq[1:50],s$spec[1:50],type='l')

tt = 1:le2

m = rbind(pn,tt)

m = as.data.frame(t(m))
colnames(m) = c('pin','te')
m$te = m$te/500-1

ALFA = ggplot(m,aes(x=te,y=pin)) +
  #xlab('Tiempo : t')+ylab('Proceso : X(t)')+
  xlab(NULL)+ylab(NULL)+
  theme_bw() +
  theme(panel.background = element_rect(fill = 'transparent',colour = NA),
        plot.background = element_rect(fill = 'transparent',colour = NA))+
  scale_x_continuous(expand=c(0,0))+
  #scale_x_continuous(breaks=NULL)+
  #scale_x_continuous(expand=c(0,0),breaks=NULL)+
  theme(axis.text.x = element_blank())+
  expand_limits(x=-1,1)+
  geom_line()

# grabar
#ggsave('ruido_alfa.pdf',device='pdf',dpi=600,
#       width = 15,height = 8,unit='cm')

###########################################################
#setwd('C:/Users/EQUIPO 1/Desktop/julio/Tesis_respaldo/TESIS/img_mas_ejemplos')

set.seed(2018)

a = noise(kind=c('power'),alpha = 1,duration = 2,samp.rate = 500,
          xunit='time')
pn = a@left

le2 = length(pn)

tt = 1:le2

m = rbind(pn,tt)

m = as.data.frame(t(m))
colnames(m) = c('pin','te')
m$te = (m$te-1)/500-1

ROSA = ggplot(m,aes(x=te,y=pin)) +
  #xlab('Tiempo : t')+ylab('Proceso : X(t)')+
  xlab(NULL)+ylab(NULL)+
  theme_bw() +
  theme(panel.background = element_rect(fill = 'transparent',colour = NA),
        plot.background = element_rect(fill = 'transparent',colour = NA))+
  scale_x_continuous(expand=c(0,0))+
  #scale_x_continuous(breaks=NULL)+
  #theme(axis.text.x = element_blank())+
  expand_limits(x=1)+
  geom_line()

# grabar
#ggsave('ruido_rosa.pdf',device='pdf',dpi=600,
#       width = 15,height = 8,unit='cm')

###########################################################

set.seed(2018)

pn = rnorm(1001)

le2 = length(pn)

tt = 1:le2

m = rbind(pn,tt)

m = as.data.frame(t(m))
colnames(m) = c('pin','te')
m$te = (m$te-1)/500-1

BLANCO = ggplot(m,aes(x=te,y=pin)) +
  #xlab('Tiempo : t')+ylab('Proceso : X(t)')+
  xlab(NULL)+ylab(NULL)+
  theme_bw() +
  theme(panel.background = element_rect(fill = 'transparent',colour = NA),
        plot.background = element_rect(fill = 'transparent',colour = NA))+
  scale_x_continuous(expand=c(0,0))+
  expand_limits(x=1)+
  #scale_x_continuous(breaks=NULL)+
  #scale_x_continuous(expand=c(0,0),breaks=NULL)+
  theme(axis.text.x = element_blank())+
  geom_line()

# grabar
#ggsave('ruido_rosa.pdf',device='pdf',dpi=600,
#       width = 15,height = 8,unit='cm')

###########################################################

set.seed(2017)

tt = (0:1000)/500-1

phi = runif(1,0,2*pi)

pn = sin(pi*tt+phi)

m = rbind(pn,tt,tt*0+1)

phi = runif(1,0,2*pi)

pn = sin(pi*tt+phi)

m = cbind(m,rbind(pn,tt,tt*0+2))

phi = runif(1,0,2*pi)

pn = sin(pi*tt+phi)

m = cbind(m,rbind(pn,tt,tt*0+3))

m = as.data.frame(t(m))
colnames(m) = c('pin','te','ind')
#m$te = (m$te-1)/500-1
m$ind = factor(m$ind)

OSCI = ggplot(m,aes(x=te,y=pin,linetype=ind)) +
  #xlab('Tiempo : t')+ylab('Proceso : X(t)')+
  xlab(NULL)+ylab(NULL)+
  theme_bw() +
  theme(panel.background = element_rect(fill = 'transparent',colour = NA),
        plot.background = element_rect(fill = 'transparent',colour = NA))+
  scale_x_continuous(expand=c(0,0))+
  #scale_x_continuous(breaks=NULL)+
  #scale_x_continuous(expand=c(0,0),breaks=NULL)+
  theme(axis.text.x = element_blank())+
  expand_limits(x=1)+
  theme(legend.position = 'none')+
  geom_line()

# grabar
#ggsave('ruido_rosa.pdf',device='pdf',dpi=600,
#       width = 15,height = 8,unit='cm')

###########################################################

set.seed(2018)

le1 = 100
le2 = 1000

ee = rnorm(le2 + 2*le1 + 1)

tt = (1:le1)
gg = tt*0 + 1/(2*le1+1)
#gg = sin(tt/30)/tt-sin(tt/12)/tt
gg = c(rev(gg),0,gg)

#gg = ((0.049922035 -0.095993537/(tt) +0.050612699/(tt**2) -0.004408786/(tt**3))/
#        (1 -2.494956002/(tt) +2.017265875/(tt**2) -0.522189400/(tt**3)))

le1 = length(gg)

pn = rep(0,le2)

for(i in 1:le2){
  pn[i] = sum(gg*ee[1:le1+ i-1])
}

s = pspectrum(pn,x.frqsamp = 500 ,Nyquist.normalize = F)
plot(pn,type='l')
plot(s$freq[1:50],s$spec[1:50],type='l')

tt = 1:le2

m = rbind(pn,tt)

m = as.data.frame(t(m))
colnames(m) = c('pin','te')
m$te = m$te/500-1

MA = ggplot(m,aes(x=te,y=pin)) +
  #xlab('Tiempo : t')+ylab('Proceso : X(t)')+
  xlab(NULL)+ylab(NULL)+
  theme_bw() +
  theme(panel.background = element_rect(fill = 'transparent',colour = NA),
        plot.background = element_rect(fill = 'transparent',colour = NA))+
  scale_x_continuous(expand=c(0,0))+
  #scale_x_continuous(breaks=NULL)+
  #scale_x_continuous(expand=c(0,0),breaks=NULL)+
  theme(axis.text.x = element_blank())+
  expand_limits(x=-1,1)+
  geom_line()

# grabar
#ggsave('ruido_alfa.pdf',device='pdf',dpi=600,
#       width = 15,height = 8,unit='cm')

###########################################################

ggarrange(OSCI,BLANCO,MA,ALFA,ROSA,ncol=1,nrow=5,align = 'v',
          labels='AUTO')

ggsave('ruidos_ejemplos.pdf',device='pdf',dpi=600,
       width = 15,height = 16,unit='cm')
