  p.val   = .1   # p-valor de rechazo para la hipotesis
  grabar = F
  
  matriz_mor = matrix(nrow=11,ncol=22)
  
  colnames(matriz_mor)=channel
  row.names(matriz_mor) = nomb_facil
  
  central_dir = 'C:/Users/Erika/Desktop/Julio161213/scripts170308'
  setwd(central_dir)
  
  for(sujeto in 1:11){
    setwd(central_dir)
    source('porcentajes03_PDC_sinfiltro.R')
  }
  
  m_mor_NN = matriz_mor[(grupo_de==0),]
  m_mor_MN = matriz_mor[(grupo_de==1),]
  
  
  n_NN = sum((grupo_de==0)*1)
  n_MN = sum((grupo_de==1)*1)
  
  ################
  
  MINI = floor(20*min(min(m_mor_NN),min(m_mor_MN)))/20
  MAXI = ceiling(20*max(max(m_mor_NN),max(m_mor_MN)))/20
  
  plot(m_mor_NN[1,],type='l',col='white',ylim=c(MINI,MAXI),
       xaxt='n',ylab='% epocas estacionaeriedad',xlab='',
       main=paste0('% estacionariedad , sin filtro , *=',p.val))
  
  axis(1,at=1:22,labels=(channel),las=2,
       tick=T)
  
  for(i in 1:n_NN){
    lines(m_mor_NN[i,],type='l',col='blue')
    #lines(m_mor_NN[i,],type='o',col='blue',pch=19)
  }
  for(i in 1:n_MN){
    lines(m_mor_MN[i,],type='l',col='red')
    #lines(m_mor_MN[i,],type='o',col='red',pch=19)
  }
  
  ################
  
  promedios = matrix(nrow=2,ncol=22)
  varianzas = matrix(nrow=2,ncol=22)
  
  for(ch in 1:22){
    promedios[1,ch] = mean(m_mor_NN[,ch])
    promedios[2,ch] = mean(m_mor_MN[,ch])
    varianzas[1,ch] = sd(m_mor_NN[,ch])
    varianzas[2,ch] = sd(m_mor_MN[,ch])
  }
  
  #MINI = floor(20*min(c(promedios[1,],promedios[2,])))/20
  #MAXI = ceiling(20*max(c(promedios[1,],promedios[2,])))/20
  
  #plot(promedios[1,],type='l',ylim=c(MINI,MAXI))
  #lines(promedios[2,],type='l',ylim=c(MINI,MAXI))
  
  lines(promedios[1,],type='l',col='black',lwd=2)
  lines(promedios[1,],type='o',col='black',pch=19)
  
  lines(promedios[2,],type='l',col='purple',lwd=2)
  lines(promedios[2,],type='o',col='purple',pch=19)
  
  legend('topright',
         legend=c('Normal','PDC',
                  'Normal (promedio)','PDC (promedio)'),
         col=c('blue','red','black','purple'),
         lty=1,lwd=2)
  
  ####
  