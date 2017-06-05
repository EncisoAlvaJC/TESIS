grabar = T

setwd("~/TESIS/TESIS/img_diagramas")

set.seed(2017)

N = 24000

S = 10

P = rpois(N,S)

C = c(rep(0,N/12),rep(1,N/12),
      rep(0,N/12),rep(1,N/12),
      rep(0,N/12),rep(1,N/12),
      rep(0,N/12),rep(1,N/12),
      rep(0,N/12),rep(1,N/12),
      rep(0,N/12),rep(1,N/12))

for(i in 1:N){
  if(C[i]>0){
    P[i] = S
  }
}

X = rnorm(N,0,1)*P*(1/S)

if(grabar){
  #setwd(g_dir)
  pdf(paste0('espectrosA',
             '.pdf'),width=10,height=3)
  #'.png'),units='in',res=150,width=12,height=6)
}

par(mar=c(4,4,2,1))

plot((1:N)/part,X,type='l',col='black',
     xlab='t',ylab='mV',main='Sujeto: ---')

if(grabar){
  dev.off()
  
  #setwd(g_dir)
  pdf(paste0('espectrosB',
             '.pdf'),width=5.5,height=2.5)
  #'.png'),units='in',res=150,width=12,height=6)
}

par(mar=c(4,4,2,1))

part = 4

plot((1:N)/part,X,type='l',col='white',
     xlab='t',ylab='mV',main='Sujeto: --- (30 s)')

for(i in 0:(part/2-1)){
  sub = 2*i*(N/part)
  ind = (sub+1):(sub+N/part)
  lines(ind/part,X[ind],type='l',col='cadetblue4',xlab='',ylab='')
  sub = (2*i+1)*(N/part)
  ind = (sub+1):(sub+N/part)
  lines(ind/part,X[ind],type='l',col='chartreuse4',xlab='',ylab='')
}

if(grabar){
  dev.off()
  
  #setwd(g_dir)
  pdf(paste0('espectrosC',
             '.pdf'),width=5.5,height=2.5)
  #'.png'),units='in',res=150,width=12,height=6)
}

par(mar=c(4,4,2,1))

part = 12

plot((1:N)/part,X,type='l',col='white',
     xlab='t',ylab='mV',main='Sujeto: --- (10 s)')

for(i in 0:(part/2-1)){
  sub = 2*i*(N/part)
  ind = (sub+1):(sub+N/part)
  lines(ind/part,X[ind],type='l',col='cadetblue4',xlab='',ylab='')
  sub = (2*i+1)*(N/part)
  ind = (sub+1):(sub+N/part)
  lines(ind/part,X[ind],type='l',col='chartreuse4',xlab='',ylab='')
}

if(grabar){
  dev.off()
  
  #setwd(g_dir)
  pdf(paste0('espectrosD',
             '.pdf'),width=5.5,height=2)
  #'.png'),units='in',res=150,width=12,height=6)
}

library(plotrix)

M = matrix(c(1,1,1,1))
M = t(M)

par(mar=c(4,2,2,1))

color2D.matplot(M,axes=F,xlab='t',ylab='')

if(grabar){
  dev.off()
  
  #setwd(g_dir)
  pdf(paste0('espectrosE',
             '.pdf'),width=5.5,height=2)
  #'.png'),units='in',res=150,width=12,height=6)
}

M = matrix(c(1,0,1,0,1,0,1,0,1,0,1,0))
M = t(M)

par(mar=c(4,2,2,1))

color2D.matplot(M,axes=F,xlab='t',ylab='')

if(grabar){
  dev.off()
}
