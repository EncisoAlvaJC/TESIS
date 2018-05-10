###########################################################
## Procesos no-estacionarios ##############################

require('ggpubr')
require('scales')
require('tuneR')
require('psd')

require('reshape')

###########################################################
# proceso alfa modulado

set.seed(2018)

mini = -4
maxi =  4

tmp  = seq(mini,maxi,by=1/500)
lon  = length(tmp)

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
  theme(axis.text.x = element_blank())+
  geom_line()
plot(ALFA)


m = rbind(X.t,tmp)

m = as.data.frame(t(m))
colnames(m) = c('proceso','tiempo')

MACH2 = ggplot(m,aes(x=tiempo,y=proceso)) +
  xlab(NULL)+ylab(NULL)+
  theme_bw() +
  theme(panel.background = element_rect(fill = 'transparent',colour = NA),
        plot.background = element_rect(fill = 'transparent',colour = NA))+
  scale_x_continuous(expand=c(0,0))+
  theme(axis.text.x = element_blank())+
  geom_line()

# covarianza

# t = -2
cov1 = rep(0,len)
for(i in 1:len){
  cov1[i] = phi(-2) + phi(tau[i]-2) - abs(tau[i])
}
cov1[cov1<0] = 0
plot(tau-2,cov1,type='l')

# t = -1
cov1 = rep(0,len)
for(i in 1:len){
  cov1[i] = phi(-1) + phi(tau[i]-1) - abs(tau[i])
}
cov1[cov1<0] = 0
plot(tau-1,cov1,type='l')

# t = 0
tau  = seq(-2,2,by=1/500)
len  = length(tau)

cov1 = rep(0,len)
for(i in 1:len){
  cov1[i] = phi(0) + phi(tau[i]) - abs(tau[i])
}
cov1[cov1<0] = 0
plot(tau,cov1,type='l')

# t = 1
cov1 = rep(0,len)
for(i in 1:len){
  cov1[i] = phi(1) + phi(tau[i]+1) - abs(tau[i])
}
cov1[cov1<0] = 0
plot(tau+1,cov1,type='l')

# t = 2
cov1 = rep(0,len)
for(i in 1:len){
  cov1[i] = phi(2) + phi(tau[i]+2) - abs(tau[i])
}
cov1[cov1<0] = 0
plot(tau+2,cov1,type='l')

###########################################################
# proceso MA cambiante, anchi de banda finito

phi = function(t){
  return((tanh(t)+1)/2)
}

set.seed(2018)

mini = -4
maxi =  4

tmp  = seq(mini,maxi,by=1/500)
lon  = length(tmp)

X.t = rep(0,lon)

lon2 = ceiling(phi(mini)*500) + ceiling(phi(maxi)*500) + 1
t.0  = ceiling(phi(mini)*500)

eps = rnorm( lon + lon2 )

for( i in 1:lon ){
  t = tmp[i]
  
  t.ini = t.0 + i - ceiling(phi(t)*500)
  t.fin = t.0 + i + ceiling(phi(t)*500)
  
  X.t[i] = sum(eps[t.ini:t.fin])#/(t.fin-t.ini)
}

for( i in 1:lon ){
  t = tmp[i]
  
  X.t[i] = X.t[i]/sqrt(2*phi(t))
}

m = rbind(X.t,tmp)

m = as.data.frame(t(m))
colnames(m) = c('proceso','tiempo')

MACH1 = ggplot(m,aes(x=tiempo,y=proceso)) +
  xlab(NULL)+ylab(NULL)+
  theme_bw() +
  theme(panel.background = element_rect(fill = 'transparent',colour = NA),
        plot.background = element_rect(fill = 'transparent',colour = NA))+
  scale_x_continuous(expand=c(0,0))+
  theme(axis.text.x = element_blank())+
  geom_line()

# covarianza

# t = -2
cov1 = rep(0,len)
for(i in 1:len){
  cov1[i] = phi(-2) + phi(tau[i]-2) - abs(tau[i])
}
cov1[cov1<0] = 0
plot(tau-2,cov1,type='l')

# t = -1
cov1 = rep(0,len)
for(i in 1:len){
  cov1[i] = phi(-1) + phi(tau[i]-1) - abs(tau[i])
}
cov1[cov1<0] = 0
plot(tau-1,cov1,type='l')

# t = 0
tau  = seq(-2,2,by=1/500)
len  = length(tau)

cov1 = rep(0,len)
for(i in 1:len){
  cov1[i] = phi(0) + phi(tau[i]) - abs(tau[i])
}
cov1[cov1<0] = 0
plot(tau,cov1,type='l')

# t = 1
cov1 = rep(0,len)
for(i in 1:len){
  cov1[i] = phi(1) + phi(tau[i]+1) - abs(tau[i])
}
cov1[cov1<0] = 0
plot(tau+1,cov1,type='l')

# t = 2
cov1 = rep(0,len)
for(i in 1:len){
  cov1[i] = phi(2) + phi(tau[i]+2) - abs(tau[i])
}
cov1[cov1<0] = 0
plot(tau+2,cov1,type='l')

###########################################################
# proceso MA cambiante, cuadritos

phi = function(t){
  if(floor(t/2)*2-floor(t)==0){
    return(1/8)
  }
  return(1/64)
}

set.seed(2018)

mini = -4
maxi =  4

tmp  = seq(mini,maxi,by=1/500)
|lon  = length(tmp)

X.t = rep(0,lon)

lon2 = ceiling(phi(mini)*500) + ceiling(phi(maxi)*500) + 1
t.0  = ceiling(phi(mini)*500)

eps = rnorm( lon + lon2 )

for( i in 1:lon ){
  t = tmp[i]
  
  t.ini = t.0 + i - ceiling(phi(t)*500)
  t.fin = t.0 + i + ceiling(phi(t)*500)
  
  X.t[i] = sum(eps[t.ini:t.fin])/(t.fin-t.ini)
}

#for( i in 1:lon ){
#  t = tmp[i]
#  
#  X.t[i] = X.t[i]/sqrt(2*phi(t))
#}

m = rbind(X.t,tmp)

m = as.data.frame(t(m))
colnames(m) = c('proceso','tiempo')

CUADR = ggplot(m,aes(x=tiempo,y=proceso)) +
  xlab(NULL)+ylab(NULL)+
  theme_bw() +
  theme(panel.background = element_rect(fill = 'transparent',colour = NA),
        plot.background = element_rect(fill = 'transparent',colour = NA))+
  scale_x_continuous(expand=c(0,0))+
  #theme(axis.text.x = element_blank())+
  geom_line()
plot(CUADR)


###########################################################

# graficos de los procesos
ggarrange(WIENER,MACH1,MACH2,CUADR,ncol=1,nrow=4,align = 'v',
          labels='AUTO')
ggsave('ruidos_ejemplos_nest.pdf',device='pdf',dpi=600,
       width = 15,height = 16,unit='cm')
