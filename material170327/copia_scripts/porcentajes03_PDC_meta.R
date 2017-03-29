  len = 2
  p.val = 0.05

  grabar = F
  
  # constantes genericas
  channel   = c('C3','C4','CZ',
                'F3','F4','F7','F8',
                'FP1','FP2','FZ',
                'O1','O2','P3','P4','PZ',
                'T3','T4','T5','T6',
                'LOG','ROG',
                'EMG'
  )
  
  # nombres de los archivos/directorios con los datos
  nomb_dir  = c('CLMN10SUE',
                'JANASUE',
                'JGMN6SUE',
                'MJNNVIGILOScCanal',
                'RLMN',
                'RRMNS_2',
                'VCNNS',
                'FGH_EEGdescompuesto',
                'GURM',
                'EMNN',
                'MGNA')
  nomb_arch = c('CLMN10SUE',
                'JANASUE',
                'JGMN6SUE',
                'MJNNVIGILOS',
                'RLMN10SUE',
                'RRMNS',
                'VCNNS1',
                'FGHSUE',
                'GH24031950SUEÑO',
                'EMNNS',
                'MGNA5SUE')
  nomb_facil = c('CLMN',
                 'JANA',
                 'JGMN',
                 'MJNN',
                 'RLMN',
                 'RRMN',
                 'VCNN',
                 'FGH',
                 'GURM',
                 'EMNN',
                 'MGNA')
  
  matriz_mor = matrix(nrow=11,ncol=22)
  matriz_nmor = matrix(nrow=11,ncol=22)
  matriz_tot = matrix(nrow=11,ncol=22)
    
  colnames(matriz_mor)=channel
  row.names(matriz_mor) = nomb_facil
  
  central_dir = 'C:/Users/Erika/Desktop/Julio161213/scripts170308'
  save_dir    = 'C:/Users/Erika/Desktop/Julio161213/scripts170308/neodata'
  
  setwd(central_dir)
  
  for(sujeto in 1:11){
    setwd(central_dir)
    #source('porcentajes03_PDC_nMOR_epocasclasicas.R')
    source('porcentajes03_PDC.R')
  }
  
  m_mor_NN = matriz_mor[(grupo_de==0),]
  m_mor_MN = matriz_mor[(grupo_de==1),]
  
  m_nmor_NN = matriz_nmor[(grupo_de==0),]
  m_nmor_MN = matriz_nmor[(grupo_de==1),]
  
  m_tot_NN = matriz_tot[(grupo_de==0),]
  m_tot_MN = matriz_tot[(grupo_de==1),]
  
  #a = t(matriz_mor)
  #colnames(a)=nomb_facil
  #b = t(matriz_nmor)
  #colnames(b)=nomb_facil
  #c = t(matriz_tot)
  #colnames(c)=nomb_facil
  
  n_NN = sum((grupo_de==0)*1)
  n_MN = sum((grupo_de==1)*1)
  
  ################
  ################
  
  significados = rep(0,22)
  
  for(ch in 1:22){
    tt = t.test(m_mor_NN[,ch],m_mor_MN[,ch],equal.variances=F)
    #tt = wilcox.test(m_mor_NN[,ch],m_mor_MN[,ch])
    significados[ch] = as.numeric(tt['p.value'])
  }
  
  MINI = floor(20*min(min(m_mor_NN),min(m_mor_MN)))/20
  MAXI = ceiling(20*max(max(m_mor_NN),max(m_mor_MN)))/20
  
  plot(m_mor_NN[1,],type='l',col='white',ylim=c(MINI,MAXI),
       xaxt='n',ylab='% epocas estacionaeriedad',xlab='',
       main=paste0('% estacionariedad (MOR) , *=',p.val))
  
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
  
  pe = t(promedios)
  
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
         lty=1,lwd=2,cex=0.5)
  
  ########
  ########
  
  strst=(MAXI-MINI)/40
  
  for(i in 1:22){
    if(is.nan(significados[i])){
      significados[i] = 1
    }
  }
  
  ch_lin = 1:22
  equis = (significados<.1)
  lines(ch_lin[equis],rep(MAXI        ,sum(equis*1)),
        type='p',pch='*',lwd=0,cex=2)
  equis = (significados<.05)
  lines(ch_lin[equis],rep(MAXI-  strst,sum(equis*1)),
        type='p',pch='*',lwd=0,cex=2)
  equis = (significados<.01)
  lines(ch_lin[equis],rep(MAXI-2*strst,sum(equis*1)),
        type='p',pch='*',lwd=0,cex=2)
  
  ################
  ################
  
  ################
  ################
  
  for(ch in 1:22){
    tt = t.test(m_nmor_NN[,ch],m_nmor_MN[,ch],equal.variances=F)
    significados[ch] = as.numeric(tt['p.value'])
  }
  
  MINI = floor(20*min(min(m_nmor_NN),min(m_nmor_MN)))/20
  MAXI = ceiling(20*max(max(m_nmor_NN),max(m_nmor_MN)))/20
  
  plot(m_nmor_NN[1,],type='l',col='white',ylim=c(MINI,MAXI),
       xaxt='n',ylab='% epocas estacionaeriedad',xlab='',
       main=paste0('% estacionariedad (no-MOR) , *=',p.val))
  
  axis(1,at=1:22,labels=(channel),las=2,
       tick=T)
  
  for(i in 1:n_NN){
    lines(m_nmor_NN[i,],type='l',col='blue')
    #lines(m_mor_NN[i,],type='o',col='blue',pch=19)
  }
  for(i in 1:n_MN){
    lines(m_nmor_MN[i,],type='l',col='red')
    #lines(m_mor_MN[i,],type='o',col='red',pch=19)
  }
  
  ################
  
  promedios = matrix(nrow=2,ncol=22)
  varianzas = matrix(nrow=2,ncol=22)
  
  for(ch in 1:22){
    promedios[1,ch] = mean(m_nmor_NN[,ch])
    promedios[2,ch] = mean(m_nmor_MN[,ch])
    varianzas[1,ch] = sd(m_nmor_NN[,ch])
    varianzas[2,ch] = sd(m_nmor_MN[,ch])
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
         lty=1,lwd=2,cex=0.5)
  
  ########
  ########
  
  strst=(MAXI-MINI)/40
  
  for(i in 1:22){
    if(is.nan(significados[i])){
      significados[i] = 1
    }
  }
  
  ch_lin = 1:22
  equis = (significados<.1)
  lines(ch_lin[equis],rep(MAXI        ,sum(equis*1)),
        type='p',pch='*',lwd=0,cex=2)
  equis = (significados<.05)
  lines(ch_lin[equis],rep(MAXI-  strst,sum(equis*1)),
        type='p',pch='*',lwd=0,cex=2)
  equis = (significados<.01)
  lines(ch_lin[equis],rep(MAXI-2*strst,sum(equis*1)),
        type='p',pch='*',lwd=0,cex=2)
  
  ################
  ################
  
  ################
  ################
  
  for(ch in 1:22){
    tt = t.test(m_tot_NN[,ch],m_tot_MN[,ch],equal.variances=F)
    significados[ch] = as.numeric(tt['p.value'])
  }
  
  MINI = floor(20*min(min(m_tot_NN),min(m_tot_MN)))/20
  MAXI = ceiling(20*max(max(m_tot_NN),max(m_tot_MN)))/20
  
  plot(m_tot_NN[1,],type='l',col='white',ylim=c(MINI,MAXI),
       xaxt='n',ylab='% epocas estacionaeriedad',xlab='',
       main=paste0('% estacionariedad (total) , *=',p.val))
  
  axis(1,at=1:22,labels=(channel),las=2,
       tick=T)
  
  for(i in 1:n_NN){
    lines(m_tot_NN[i,],type='l',col='blue')
    #lines(m_tot_NN[i,],type='o',col='blue',pch=19)
  }
  for(i in 1:n_MN){
    lines(m_tot_MN[i,],type='l',col='red')
    #lines(m_tot_MN[i,],type='o',col='red',pch=19)
  }
  
  ################
  
  promedios = matrix(nrow=2,ncol=22)
  varianzas = matrix(nrow=2,ncol=22)
  
  for(ch in 1:22){
    promedios[1,ch] = mean(m_tot_NN[,ch])
    promedios[2,ch] = mean(m_tot_MN[,ch])
    varianzas[1,ch] = sd(m_tot_NN[,ch])
    varianzas[2,ch] = sd(m_tot_MN[,ch])
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
         lty=1,lwd=2,cex=0.5)
  
  ########
  ########
  
  strst=(MAXI-MINI)/40
  
  for(i in 1:22){
    if(is.nan(significados[i])){
      significados[i] = 1
    }
  }
  
  ch_lin = 1:22
  equis = (significados<.1)
  lines(ch_lin[equis],rep(MAXI        ,sum(equis*1)),
        type='p',pch='*',lwd=0,cex=2)
  equis = (significados<.05)
  lines(ch_lin[equis],rep(MAXI-  strst,sum(equis*1)),
        type='p',pch='*',lwd=0,cex=2)
  equis = (significados<.01)
  lines(ch_lin[equis],rep(MAXI-2*strst,sum(equis*1)),
        type='p',pch='*',lwd=0,cex=2)
  
  ################
  ################
  
  
  