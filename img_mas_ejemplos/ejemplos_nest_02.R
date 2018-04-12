###########################################################
## Procesos no-estacionarios ##############################

require('ggpubr')
require('scales')
require('tuneR')
require('psd')

###########################################################
# proceso de wiener atenuado

set.seed(2018)

tmp = seq(0,4,by=1/500)
lon = length(tmp)

eps = rnorm(lon)
X.t = rep(0,lon)

for(i in 2:lon){
  X.t[i] = X.t[i-1] + sqrt(1/500)*eps[i]
}

X.t    = X.t/sqrt(tmp)
X.t[1] = 0

m = rbind(X.t,tmp)

m = as.data.frame(t(m))
colnames(m) = c('proceso','tiempo')

WIENER = ggplot(m,aes(x=tiempo,y=proceso)) +
  xlab(NULL) + ylab('Wiener') +
  theme_bw() +
  theme(axis.title = element_text(colour = 'white'))+
  theme(panel.background = element_rect(fill = 'transparent',colour = NA),
        plot.background = element_rect(fill = 'transparent',colour = NA))+
  scale_x_continuous(expand=c(0,0))+
  #theme(axis.text.x = element_blank())+
  geom_line()

# covarianza

require('rgl')

tt  = seq(0,4,by=1/5)
ss  = seq(0,4,by=1/5)

l.t = length(tt)
l.s = length(ss)

RR = matrix(nrow=l.t,ncol=l.s)

for(i in 1:l.t){
  for(j in 1:l.s){
    if( ss[j] <= 0 ){
      RR[i,j] = 0
    }else{
      if( ss[j] <= tt[i] ){
        RR[i,j] = sqrt(ss[j]/tt[i])
      }else{
        RR[i,j] = sqrt(tt[i]/ss[j])
      }
    }
  }
}

require('plot3D')

persp3D(tt,ss,RR,border='black',shade=.1,theta=-45,phi=45,
        bty = 'b2',col=NULL,
        xlab='t',ylab='s',zlab='Autocov.',
        cex=10,ticktype = "detailed",zlim=c(0,1.5))
        #contour = list(side = c("zmax+1", "z")),
        #contour = TRUE, image=TRUE)
persp3D(tt,ss,RR,border=NULL,shade=.1,theta=-45,phi=45,
        bty = 'b2',col='lightblue',facets=NA,
        xlab='t',ylab='s',zlab='Autocov.',
        cex=10,ticktype = "detailed",zlim=c(0,1.5),
        contour = TRUE, image=TRUE)


persp3D(tt,ss,RR,border=NA,shade=.1,theta=-35,phi=55,
        bty = 'b2',col=NA,
        xlab='t',ylab='s',zlab='Autocov.',
        cex=10,ticktype = "detailed",zlim=c(0,1.5),
        contour = list(side = c("zmax+1", "z"))
        #contour = TRUE
        )

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
# proceso MA cambiante, anchi de banda INFINITO

phi = function(t){
  if(t<=0){
    return(exp(t))
  }
  return(log(t+1)+1)
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
lon  = length(tmp)

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

###########################################################

# graficos de los procesos
ggarrange(WIENER,MACH1,MACH2,CUADR,ncol=1,nrow=4,align = 'v',
          labels='AUTO')
ggsave('ruidos_ejemplos_nest.pdf',device='pdf',dpi=600,
       width = 15,height = 16,unit='cm')
