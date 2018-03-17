###########################################################
## Ruidos de colores #####################################

require('ggpubr')
require('scales')

###########################################################
setwd('C:/Users/EQUIPO 1/Desktop/julio/Tesis_respaldo/TESIS/img_mas_ejemplos')
grabar = T

set.seed(2018)

le1 = 100000
le2 = 1000

ee = rnorm(le2 + 2*le1 + 1)

tt = 1:le1
gg = 2/(tt**2+1)
gg = c(rev(gg),1,gg)

le1 = length(gg)

pn = rep(0,le2)

for(i in 1:le2){
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
ggsave(file=paste0('ruido_rosa.pdf'),#width=3,height=2,
       bg='transparent',unit='cm')

###########################################################

set.seed(2018)

le1 = 100000
le2 = 1000

ee = rnorm(le2 + 2*le1 + 1)

tt = 1:le1
gg = 1/(tt+1)
gg = c(rev(gg),1,gg)

le1 = length(gg)

pn = rep(0,le2)

for(i in 1:le2){
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
ggsave(file=paste0('ruido_rosa.pdf'),#width=3,height=2,
       bg='transparent',unit='cm')
