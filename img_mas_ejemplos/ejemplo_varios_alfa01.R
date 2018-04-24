###########################################################
## Ejemplos de procesos estacionarios #####################

require('ggpubr')
require('scales')
require('psd')

###########################################################
# Proceso tipo Alfa

set.seed(2018)

tmp = seq(0,4,by=1/500)
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
  xlab(NULL)+ylab('X(t)')+
  theme_bw() +
  theme(panel.background = element_rect(fill = 'transparent',colour = NA),
        plot.background = element_rect(fill = 'transparent',colour = NA))+
  scale_x_continuous(expand=c(0,0))+
  theme(axis.text.x = element_blank())+
  geom_line()
plot(ALFA)

m$proceso1 = m$proceso * exp(-abs(m$tiempo))

ALFA1 = ggplot(m,aes(x=tiempo,y=proceso1)) +
  xlab(NULL)+ylab(expression(Y[1](t)))+
  theme_bw() +
  theme(panel.background = element_rect(fill = 'transparent',colour = NA),
        plot.background = element_rect(fill = 'transparent',colour = NA))+
  scale_x_continuous(expand=c(0,0))+
  theme(axis.text.x = element_blank())+
  geom_line()
plot(ALFA1)

m$proceso2 = m$proceso * exp(-abs(m$tiempo)**2 / 2)

ALFA2 = ggplot(m,aes(x=tiempo,y=proceso2)) +
  xlab('t')+ylab(expression(Y[2](t)))+
  theme_bw() +
  theme(panel.background = element_rect(fill = 'transparent',colour = NA),
        plot.background = element_rect(fill = 'transparent',colour = NA))+
  scale_x_continuous(expand=c(0,0))+
  #theme(axis.text.x = element_blank())+
  geom_line()
plot(ALFA2)

ggarrange(ALFA,ALFA1,ALFA2,
          ncol=1,nrow=3,align = 'v',labels='AUTO')

ggsave('proceso_alfa_varios.pdf',device='pdf',dpi=600,
       width = 15,height = 16,unit='cm')

###########################################################