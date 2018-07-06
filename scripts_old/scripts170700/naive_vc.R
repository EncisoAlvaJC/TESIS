#require(plotrix)

grabar = T

#sujeto = 1

mini = 1
#maxi = 512*30

#ch = 7

if(sujeto == 1){
  nom_dir  = 'CLMN10SUE'
  nom_arch = 'CLMN10SUE'
  etiqueta = 'CLMN'
  epoca    = 166
}else{
  nom_dir  = 'MJNNVIGILOScCanal'
  nom_arch = 'MJNNVIGILOS'
  etiqueta = 'MJNN'
  epoca    = 183
}
  
w_dir = getwd()
r_dir = paste0(getwd(),'/res')

channel = c('C3','C4','CZ','EMG','F3','F4','F7','F8',
            'FP1','FP2','FZ','LOG','O1','O2','P3','P4','PZ',
            'ROG','T3','T4','T5','T6')


canal  = channel[ch]
nom_ar = paste0(nom_arch,'_',canal,'_',epoca,'.txt')

# cargar datos
setwd(w_dir)
Y  = scan(nom_ar)

Y = Y[mini:maxi]

#Y = rnorm(length(mini:maxi))

# if(grabar){
#   setwd(r_dir)
#   png(paste0(etiqueta,'_',canal,'_',toString(epoca),
#              '_max',toString(maxi),
#              '_grafica.png'),
#       units='in',res=300,width=12,height=6)
# }

# par(mar=c(4,4,3,1))
# plot(Y,type='l',xlab='Tiempo (s/512)',ylab='mV',
#      main=paste0('Sujeto : ',etiqueta,' , Canal : ',canal))
# 
# longitud = length(Y)
# VM = matrix(0,nrow=longitud,ncol=longitud)
# for(i in 1:(longitud-2)){
#   VM[i,i  ] = 0.5
#   VM[i,i+1] = 1
#   m   = Y[i+1] - Y[i]
#   Y_t = Y[i+1]
#   
#   y_anterior = Y[i+1]
#   t_anterior = i+1
#   for(j in (i+2):longitud){
#     Y_t = Y_t + m
#     y_ajustado = Y[j] - Y_t
#     
#     #plot(Y,type='l')
#     #lines(c(i,j),c(Y[i],Y_t),col='red')
#     
#     if(y_ajustado<0){
#       next()
#     }else{
#       VM[i,j] = 1
#       Y_t = Y[j]
#       m   = (Y[j]-Y[i])/(j-i)
#     }
#   }
# }
# VM = VM + t(VM)

# if(grabar){
#   dev.off()
#   
#   setwd(r_dir)
#   png(paste0(etiqueta,'_',canal,'_',toString(epoca),
#              '_max',toString(maxi),
#              '_matriz.png'),
#       units='in',res=300,width=12,height=6)
# }

# par(mar=c(2,2,3,1))
# color2D.matplot(-VM,border=NA,na.color=NA,
#               main='Matriz de adyacencia (grafo de visibilidad)',
#               xlab='',ylab='')
# title(sub=paste0('Sujeto : ',etiqueta,' , Canal : ',canal))
# axis(2,at=c(mini-1,maxi),labels=F,tick=T)
# axis(3,at=c(mini-1,maxi),labels=F,tick=T)

# if(grabar){
#   dev.off()
#   
#   setwd(r_dir)
#   png(paste0(etiqueta,'_',canal,'_',toString(epoca),
#              '_max',toString(maxi),
#              '_histograma.png'),
#       units='in',res=300,width=12,height=6)
# }
# 
# grados = rep(0,longitud)
# for(i in 1:longitud){
#  grados[i] = sum(VM[i,])-1
# }
# par(mar=c(4,4,3,1))
# hist(grados,
#     col='blue',breaks=max(grados),
#     xlab='Grados',ylab='Frecuencia',
#     main='Grados de vertices (grafo de visibilidad)')
# title(sub=paste0('Sujeto : ',etiqueta,' , Canal : ',canal))
# 
# if(grabar){
#   dev.off()
# }

setwd(w_dir)



if(grabar){
 setwd(r_dir)
 png(paste0(etiqueta,'_',canal,'_',toString(epoca),
            '_max',toString(maxi),
            '_grafica.png'),
     units='in',res=300,width=12,height=6)
}

par(mar=c(4,4,3,1))
plot(Y,type='l',xlab='Tiempo (s/512)',ylab='mV',
     main=paste0('Sujeto : ',etiqueta,' , Canal : ',canal))

longitud = length(Y)
VM = matrix(0,nrow=longitud,ncol=longitud)
for(i in 1:(longitud-2)){
  VM[i,i  ] = 0.5
  VM[i,i+1] = 1
  m   = Y[i+1] - Y[i]
  Y_t = Y[i+1]
  
  y_anterior = Y[i+1]
  t_anterior = i+1
  for(j in (i+2):longitud){
    Y_t = Y_t + m
    y_ajustado = Y[j] - Y_t
    
    #plot(Y,type='l')
    #lines(c(i,j),c(Y[i],Y_t),col='red')
    
    if(y_ajustado<0){
      next()
    }else{
      VM[i,j] = 1
      Y_t = Y[j]
      m   = (Y[j]-Y[i])/(j-i)
    }
  }
}
VM = VM + t(VM)


if(grabar){
  dev.off()
  
  setwd(r_dir)
  png(paste0(etiqueta,'_',canal,'_',toString(epoca),
             '_max',toString(maxi),
             '_matriz.png'),
      units='in',res=600,width=12,height=6)
}

par(mar=c(2,2,3,1))
color2D.matplot(-VM,border=NA,na.color=NA,
                main='Matriz de adyacencia (grafo de visibilidad)',
                xlab='',ylab='')
title(sub=paste0('Sujeto : ',etiqueta,' , Canal : ',canal))
axis(2,at=c(mini-1,maxi),labels=F,tick=T)
axis(3,at=c(mini-1,maxi),labels=F,tick=T)



if(grabar){
  dev.off()
  
  setwd(r_dir)
  png(paste0(etiqueta,'_',canal,'_',toString(epoca),
             '_max',toString(maxi),
             '_matriz_inv.png'),
      units='in',res=300,width=12,height=6)
}

Y = -Y


# par(mar=c(4,4,3,1))
# plot(Y,type='l',xlab='Tiempo (s/512)',ylab='mV',
#      main=paste0('Sujeto : ',etiqueta,' , Canal : ',canal))

longitud = length(Y)
VMi = matrix(0,nrow=longitud,ncol=longitud)
for(i in 1:(longitud-2)){
  VMi[i,i  ] = 0.5
  VMi[i,i+1] = 1
  m   = Y[i+1] - Y[i]
  Y_t = Y[i+1]
  
  y_anterior = Y[i+1]
  t_anterior = i+1
  for(j in (i+2):longitud){
    Y_t = Y_t + m
    y_ajustado = Y[j] - Y_t
    
    #plot(Y,type='l')
    #lines(c(i,j),c(Y[i],Y_t),col='red')
    
    if(y_ajustado<0){
      next()
    }else{
      VMi[i,j] = 1
      Y_t = Y[j]
      m   = (Y[j]-Y[i])/(j-i)
    }
  }
}
VMi = VMi + t(VMi)

par(mar=c(2,2,3,1))
color2D.matplot(-VMi,border=NA,na.color=NA,
                main='Matriz de adyacencia (grafo de visibilidad)',
                xlab='',ylab='')
title(sub=paste0('Sujeto : ',etiqueta,' , Canal : ',canal))
axis(2,at=c(mini-1,maxi),labels=F,tick=T)
axis(3,at=c(mini-1,maxi),labels=F,tick=T)

if(grabar){
  dev.off()
  
  setwd(r_dir)
  png(paste0(etiqueta,'_',canal,'_',toString(epoca),
             '_max',toString(maxi),
             '_matriz_sum.png'),
      units='in',res=300,width=12,height=6)
}


par(mar=c(2,2,3,1))
color2D.matplot(-((VMi+VM)>0)*1,border=NA,na.color=NA,
                main='Matriz de adyacencia (grafo de visibilidad)',
                xlab='',ylab='')
title(sub=paste0('Sujeto : ',etiqueta,' , Canal : ',canal))
axis(2,at=c(mini-1,maxi),labels=F,tick=T)
axis(3,at=c(mini-1,maxi),labels=F,tick=T)

if(grabar){
  dev.off()
}