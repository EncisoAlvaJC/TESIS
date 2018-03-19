###########################################################
## Ruidos de colores #####################################

require('ggpubr')
require('scales')
require('tuneR')
require('psd')

###########################################################
#setwd('C:/Users/EQUIPO 1/Desktop/julio/Tesis_respaldo/TESIS/img_mas_ejemplos')

set.seed(2017)

a = noise(kind=c('power'),alpha = 1,duration = 2,samp.rate = 500,
          xunit='time')
pn = a@left

le2 = length(pn)

tt = 1:le2

m = rbind(pn,tt)

m = as.data.frame(t(m))
colnames(m) = c('pin','te')
m$te = (m$te-1)/500-1

ggplot(m,aes(x=te,y=pin)) +
  xlab('Tiempo : t')+ylab('Proceso : X(t)')+
  theme_bw() +
  theme(panel.background = element_rect(fill = 'transparent',colour = NA),
        plot.background = element_rect(fill = 'transparent',colour = NA))+
  scale_x_continuous(expand=c(0,0))+
  expand_limits(x=1)+
  geom_line(color='purple')

# grabar
ggsave('ruido_rosa.pdf',device='pdf',dpi=600,
       width = 15,height = 8,unit='cm')

###########################################################

set.seed(2017)

a = noise(kind=c('power'),alpha = 2,duration = 2,samp.rate = 500,
          xunit='time')
pn = a@left

le2 = length(pn)

tt = 1:le2

m = rbind(pn,tt)

m = as.data.frame(t(m))
colnames(m) = c('pin','te')
m$te = m$te/500-1

ggplot(m,aes(x=te,y=pin)) +
  xlab('Tiempo : t')+ylab('Proceso : X(t)')+
  theme_bw() +
  theme(panel.background = element_rect(fill = 'transparent',colour = NA),
        plot.background = element_rect(fill = 'transparent',colour = NA))+
  scale_x_continuous(expand=c(0,0))+
  expand_limits(x=c(-1,1))+
  geom_line(color='red')

# grabar
ggsave('ruido_rojo.pdf',device='pdf',dpi=600,
       width = 15,height = 8,unit='cm')

###########################################################

set.seed(2017)

nos = rep(0,1001)

for(i in 1:1001){
  set.seed(2017)
  pp = exp((i-501)*log(2)/500)
  a = noise(kind=c('power'),alpha = pp,duration = 2,samp.rate = 500,
            xunit='time')
  pn = a@left
  nos[i] = pn[i]
  #plot(pn,type='l')
  #invisible(readline(prompt="Press [enter] to continue"))
}

le2 = length(nos)

tt = 1:le2

m = rbind(nos,tt)

m = as.data.frame(t(m))
colnames(m) = c('pin','te')
m$te = m$te/500-1

ggplot(m,aes(x=te,y=pin)) +
  xlab('Tiempo : t')+ylab('Proceso : X(t)')+
  theme_bw() +
  theme(panel.background = element_rect(fill = 'transparent',colour = NA),
        plot.background = element_rect(fill = 'transparent',colour = NA))+
  scale_x_continuous(expand=c(0,0))+
  expand_limits(x=c(-1,1))+
  geom_line(color='red')

# grabar
ggsave('ruido_rojizo.pdf',device='pdf',dpi=600,
       width = 15,height = 8,unit='cm')

###########################################################

set.seed(2018)

le1 = 100000
le2 = 1000

ee = rnorm(le2 + 2*le1 + 1)

tt = (1:le1)
gg = sin(tt)/tt-sin(2*tt)/tt
#gg = sin(tt)/tt #- sin(tt*.5)/tt
gg = c(rev(gg),0,gg)

#gg = ((0.049922035 -0.095993537/(tt) +0.050612699/(tt**2) -0.004408786/(tt**3))/
#        (1 -2.494956002/(tt) +2.017265875/(tt**2) -0.522189400/(tt**3)))

le1 = length(gg)

pn = rep(0,le2)

for(i in 1:le2){
  pn[i] = sum(gg*ee[1:le1+ i-1])
}

s = pspectrum(pn,Nyquist.normalize = F)
plot(pn,type='l')
plot(s)

DFA(pn)

plot(ee[1:le2],type='l')
plot(pn,type='l')

tt = 1:le2

m = rbind(pn,tt)

m = as.data.frame(t(m))
colnames(m) = c('pin','te')
m$te = m$te/500

ggplot(m,aes(x=te,y=pin)) +
  xlab('Tiempo : t')+ylab('Proceso : X(t)')+
  theme_bw() +
  theme(panel.background = element_rect(fill = 'transparent',colour = NA),
        plot.background = element_rect(fill = 'transparent',colour = NA))+
  scale_x_continuous(expand=c(0,0))+
  geom_line(color='purple')

# grabar
ggsave('ruido_rosa.pdf',device='pdf',dpi=600,
       width = 15,height = 8,unit='cm')

###########################################################