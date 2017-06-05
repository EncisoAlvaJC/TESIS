grabar = T

setwd("~/TESIS/TESIS/img_diagramas")

set.seed(2017)

N = 50000

S = 10

P = rpois(N,S)

C = c(rep(0,N/10),rep(1,N/10),
      rep(0,N/10),rep(1,N/10),
      rep(0,N/10),rep(1,N/10),
      rep(0,N/10),rep(1,N/10),
      rep(0,N/10),rep(1,N/10))

for(i in 1:N){
  if(C[i]){
    P[i] = S
  }
}

X = rnorm(N,0,1)*P*(1/S)

if(grabar){
  #setwd(g_dir)
  pdf(paste0('espectros1',
             '.pdf'),width=8,height=2.5)
  #'.png'),units='in',res=150,width=12,height=6)
}

par(mar=c(4,4,2,1))

plot((1:N)/10,X,type='l',col='white',
     xlab='t',ylab='mV',main='Sujeto: ---')

part = 10

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
  pdf(paste0('espectros2',
             '.pdf'),width=8,height=2.5)
  #'.png'),units='in',res=150,width=12,height=6)
}

par(mar=c(3,4,2,1))

i = 2
sub = 2*i*(N/part)
ind = (sub+1):(sub+N/part)
XX = X[ind]

NN = N/part
partt = 5

arcoiris = rainbow(partt)

plot(XX,type='l',col='white',
     xlab='t',ylab='mV',main='Epoca: ---',
     xaxt='n')

indd = 1:(length(XX))

for(i in 0:(partt-1)){
  sub = i*(NN/partt)
  indd = (sub+1):(sub+NN/partt)
  lines(indd,XX[indd],type='l',
        col=arcoiris[i+1])
  axis(1,at=c(sub+NN/(partt*2)),
       labels=paste0('t=',toString(i+1)))
}

if(grabar){
  dev.off()
  
  #setwd(g_dir)
  pdf(paste0('espectros3',
             '.pdf'),width=8,height=3)
  #'.png'),units='in',res=150,width=12,height=6)
}

par(mar=c(4,3,2,1))

i = 0
sub = i*(NN/partt)
indd = (sub+1):(sub+NN/partt)
XXX = XX[ind]
plot(abs(fft(XX[indd])),col='white',
     type = 'l',ylim=c(0,400),
     xlab='w',ylab='',main='Y(t,w)',
     yaxt='n')

for(i in 0:(partt-1)){
 sub = i*(NN/partt)
 indd = (sub+1):(sub+NN/partt)
 XXX = XX[ind]
 lines(abs(fft(XX[indd]))+i*80,type = 'l',col=arcoiris[i+1])
 axis(2,at=c(40+80*i),labels=paste0('t=',toString(i+1)),las=2)
}

if(grabar){
  dev.off()
  
  #setwd(g_dir)
  pdf(paste0('espectros4',
             '.pdf'),width=8,height=2)
  #'.png'),units='in',res=150,width=12,height=6)
}

library(plotrix)

M = matrix(c(1,0,1,0,1,0,1,0,1,0))
M = t(M)

par(mar=c(4,2,2,1))

color2D.matplot(M,axes=F,xlab='t',ylab='')

if(grabar){
  dev.off()
}
