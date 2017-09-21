grabar = T

setwd("~/TESIS/TESIS/img_diagramas")

set.seed(2017)

N.base = 175
N      = N.base*12
e      = rnorm(N+10)
X      = rep(0,N)

for(grupo in 0:11){
  if(grupo-2*floor(grupo/2)>0){
    for(i in (1:(N.base))+grupo*N.base){
      if(i>1){
        X[i] = (X[i-1] + e[i])
      }
    }
  }else{
    for(i in (1:(N.base))+grupo*N.base){
      if(i>1){
        X[i] = (0.65*X[i-1] + e[i])
      }
    }
  }
}

if(grabar){
  #setwd(g_dir)
  pdf(paste0('espectrosA',
             'v2.pdf'),width=5,height=2)
  #'.png'),units='in',res=150,width=12,height=6)
}

par(cex.axis=.7, cex.lab=.8, cex.main=1)
par(oma=c(0,0,0,0))
par(mgp=c(1.75,.7,0))
par(mar=c(2.5,2.5,1.5,.25),bg='white')
par(las=2)
plot((1:N)-1,X,type='l',col='black',
     #xlab='Tiempo [mm:ss]',
     xlab='',
     ylab='Amplitud [mV]',main='Sujeto: ---  |  Canal: ---',
     xlim = c(0,N-1),xaxt='n',
     bty='n')

par(mgp=c(2,.3,0))
mtext('Tiempo [mm:ss]',side=1,las=1,line=1.5,cex=.8)

te = ((1:N))*(120/N)
mm = floor(te/60)
ss = floor(te - 60*mm)

places = ceiling(seq(0,N,by=(N)/8))
places[1] = 1

plaquitas = c()
for(y in 1:length(places)){
  i = places[y]
  if(ss[i]==0 || ss[i]==59){
    seg = '00'
  }else{
    seg=toString(ss[i])
  }
  plaquitas = c(plaquitas,
                paste0(toString(mm[i]+7),
                                ':',seg))
}

#par(mar=c(3,3,2,1),bg='white')
#par(las=2)
#plot((1:N)-1,X,type='l',col='black',
#     xlab='Tiempo [mm:ss]',
#     ylab='mV',main='Sujeto: ---  |  Canal: ---',
#     xlim = c(0,N-1),xaxt='n',
#     mgp = c(2, 1, 0),bty='n')

places[1]=0
axis(1,at=places,labels=plaquitas,las=1)

if(grabar){
  dev.off()
}


if(grabar){
  #dev.off()
  
  #setwd(g_dir)
  pdf(paste0('espectrosB',
             'v2.pdf'),width=3,height=2)
  #'.png'),units='in',res=150,width=12,height=6)
}
 
#par(mar=c(3,3,2,1))
#par(las=2)
 
part = 4

par(cex.axis=.7, cex.lab=.8, cex.main=.8)
par(oma=c(0,0,0,0))
par(mgp=c(1.75,.7,0))
par(mar=c(1.5,1.5,1.5,.25),bg='white')
par(las=2)
plot((1:N),X,type='l',col='white',
     #xlab='Tiempo [mm:ss]',
     xlab='',
     #ylab='Amplitud [mV]',
     ylab='',
     #main='Sujeto: ---  |  Canal: --- | d_e = 30s',
     main='Época = 30s',
     xlim = c(0,N-1),xaxt='n',
     bty='n')

par(mgp=c(2,.3,0))
#mtext('Tiempo [mm:ss]',side=1,las=1,line=1.5,cex=.8)
 
#plot((1:N),X,type='l',col='white',
#    xlab='Tiempo (mm:ss)',
#    ylab='mV',main='Sujeto: ---  |  Canal: ---  (30 s)',
#    xlim = c(0,N-1),xaxt='n',
#    mgp = c(2, 1, 0))

for(i in 0:(part/2-1)){
 sub = 2*i*(N/part)
 ind = (sub+1):(sub+N/part)
 lines(ind,X[ind],type='l',col='cadetblue4',xlab='',ylab='')
 sub = (2*i+1)*(N/part)
 ind = (sub+1):(sub+N/part)
 lines(ind,X[ind],type='l',col='chartreuse4',xlab='',ylab='')
}
 
axis(1,at=places,labels=plaquitas,las=1)

if(grabar){
  dev.off()
}

if(grabar){
 #dev.off()
 
 #setwd(g_dir)
 pdf(paste0('espectrosC',
            '.pdf'),width=5.5,height=2.5)
 #'.png'),units='in',res=150,width=12,height=6)
}

par(mar=c(3,3,2,1))
par(las=2)

part = 12

plot((1:N),X,type='l',col='white',
    xlab='Tiempo (mm:ss)',
    ylab='mV',main='Sujeto: ---  |  Canal: ---  (10 s)',
    xlim = c(0,N-1),xaxt='n',
    mgp = c(2, 1, 0))

for(i in 0:(part/2-1)){
 sub = 2*i*(N/part)
 ind = (sub+1):(sub+N/part)
 lines(ind,X[ind],type='l',col='cadetblue4',xlab='',ylab='')
 sub = (2*i+1)*(N/part)
 ind = (sub+1):(sub+N/part)
 lines(ind,X[ind],type='l',col='chartreuse4',xlab='',ylab='')
}

axis(1,at=places,labels=plaquitas,las=1)

if(grabar){
 dev.off()
}

#' if(grabar){
#'   #setwd(g_dir)
#'   pdf(paste0('espectrosD',
#'              '.pdf'),width=5.5,height=2)
#'   #'.png'),units='in',res=150,width=12,height=6)
#' }
#' 
#' library(plotrix)
#' 
#' M = matrix(c(1,1,1,1))
#' M = t(M)
#' 
#' par(mar=c(4,2,2,1))
#' 
#' color2D.matplot(M,axes=F,
#'                 xlab='Tiempo (mm:ss)',
#'                 ylab='',
#'                 main='Sujeto: ---')
#' 
#' axis(2,at=c(0,1),labels=F)
#' axis(2,at=c(0.5),labels=c('Canal: ---'),tick=F)
#' 
#' axis(1,
#'      at=(0:(length(plaquitas)-1))*(length(M)/(length(plaquitas)-1)),
#'      labels=plaquitas,las=1)
#' 
#' if(grabar){
#'   dev.off()
#'   
#'   #setwd(g_dir)
#'   pdf(paste0('espectrosE',
#'              '.pdf'),width=5.5,height=2)
#'   #'.png'),units='in',res=150,width=12,height=6)
#' }
#' 
#' M = matrix(c(1,0,1,0,1,0,1,0,1,0,1,0))
#' M = t(M)
#' 
#' par(mar=c(4,2,2,1))
#' 
#' color2D.matplot(M,axes=F,
#'                 xlab='Tiempo (mm:ss)',
#'                 ylab='',
#'                 main='Sujeto: ---')
#' 
#' axis(2,at=c(0,1),labels=F)
#' axis(2,at=c(0.5),labels=c('Canal: ---'),tick=F)
#' 
#' axis(1,
#'      at=(0:(length(plaquitas)-1))*(length(M)/(length(plaquitas)-1)),
#'      labels=plaquitas,las=1)
#' 
#' if(grabar){
#'   dev.off()
#' }