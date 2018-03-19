###########################################################
## Ruidos de colores #####################################

require('ggpubr')
require('scales')

###########################################################
#setwd('C:/Users/EQUIPO 1/Desktop/julio/Tesis_respaldo/TESIS/img_mas_ejemplos')

k = .5

set.seed(2019)

le1 = 100000
le2 = 1000

ee = rnorm(le2 + 2*le1 + 1)

tt = 1:le1
gg = k*sqrt(2/pi)/(tt+k**2)
gg = c(rev(gg),1,gg)

gg = ((0.049922035 -0.095993537/(tt) +0.050612699/(tt**2) -0.004408786/(tt**3))/
        (1 -2.494956002/(tt) +2.017265875/(tt**2) -0.522189400/(tt**3)))

le1 = length(gg)

pn = rep(0,le2)

for(i in 1:le2){
  pn[i] = sum(gg*ee[1:le1+ i-1])
}

s = pspectrum(pn)
plot(pn,type='l')
lineal = lm(log(s$spec)~s$freq)
print(lineal)
plot(s)
plot(s$freq,(log(s$spec)),type='l')
lines(s$freq,(lineal$coefficients[1]+s$freq*lineal$coefficients[2]),
      type='l',col='red')

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

set.seed(2019)

le1 = 100000
le2 = 1000

ee = rnorm(le2 + 2*le1 + 1)

k=1
tt = 1:le1
gg = k*sqrt(2/pi)/(tt**2+k**2)
gg = c(rev(gg),1,gg)

le1 = length(gg)

pn = rep(0,le2)

for(i in 1:le2){
  k = exp()
  gg = k*sqrt(2/pi)/(tt**2+k**2)
  gg = c(rev(gg),1,gg)
  
  pn[i] = sum(gg*ee[1:le1+ i-1])
}

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