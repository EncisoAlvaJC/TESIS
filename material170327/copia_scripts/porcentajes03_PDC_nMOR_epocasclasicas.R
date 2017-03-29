  # libreria especifica para el grafico tipo matriz
  library(plotrix)
  
  #####

  # nombres de las carpetas
  #       data  directorio donde estan los datos
  #    central  directorio donde guardar los graficos, debe
  #             contener el subdirectorio con las epocas
  #data_dir    = 'C:/Users/Erika/Desktop/Julio161213/scripts170308/estacionariedad_n_30s/'
  data_dir    = 'C:/Users/Erika/Desktop/Julio161213/scripts170122/estacionariedad_'
  central_dir = 'C:/Users/Erika/Desktop/Julio161213/scripts170308'
  
#   # nombres de los archivos/directorios con los datos
#   nomb_dir  = c('CLMN10SUE',
#                 'JANASUE',
#                 'JGMN6SUE',
#                 'MJNNVIGILOScCanal',
#                 'RLMN',
#                 'RRMNS_2',
#                 'VCNNS',
#                 'FGH_EEGdescompuesto',
#                 'GURM',
#                 'EMNN',
#                 'MGNA')
#   nomb_arch = c('CLMN10SUE',
#                 'JANASUE',
#                 'JGMN6SUE',
#                 'MJNNVIGILOS',
#                 'RLMN10SUE',
#                 'RRMNS',
#                 'VCNNS1',
#                 'FGHSUE',
#                 'GH24031950SUEÑO',
#                 'EMNNS',
#                 'MGNA5SUE')
#   nomb_facil = c('CLMN',
#                  'JANA',
#                  'JGMN',
#                  'MJNN',
#                  'RLMN',
#                  'RRMN',
#                  'VCNN',
#                  'FGH',
#                  'GURM',
#                  'EMNN',
#                  'MGNA')
  
  grupo_de = c(1,0,1,0,1,1,0,-1,0,-1,-1)

  frecuenciasss = c(512,512,512,
                  512,512,
                  200,200,
                  512,
                  200,
                  512,512)
  frecuencia = frecuenciasss[sujeto]
  # los grupos son
  #      1  : control (normal normal)
  #      2  : 
  #      3  : deterioro cog, normal
  #      2  : 
    
  #####
  #####
  #####
  
  # control manual, paso intermedio a la automatizacion
  #sujeto  = 1     # numero de sujeto, en orden alfabetico 
  #p.val   = .01   # p-valor de rechazo para la hipotesis
  #grabar  = F     # guardado automatico de los graficos
  porcent = T     # cantidad total o proporcional
  escala  = F     # el porcentaje se grafica entre 0 y 1
  
  #####
  #####
  #####
  
  # nombres de los archivos con epocas de diferentes longitudes
  nom_len = c('60s','30s','15s','10s','05s','02_5s')
  
  # VARIABLES PARA RETIRAR
  # control manual de epocas graficadas
  binario = T   # contraste acepta/rechaza estacionariedad
  nuevo   = T   # en ves de RES(ultado) usa EST(acionario)
  
  # automatizacion de directorio datos segun sujeto
  nombre_abreviado = nomb_facil[sujeto]
  nombre  = nomb_arch[sujeto]
  nom_dir = nomb_dir[sujeto]
  
  # directorios de trabajo, nombres abreviados
  #d_dir   = paste0(data_dir,nom_dir)      # d(atos)
  e_dir   = paste0(central_dir,'/epocas2') # e(pocas)
  r_dir   = central_dir                   # r(esultados)
  g_dir   = paste0(central_dir,
                   '/grafiquitos')# g(raficos)
  
  #####
  
  # constantes genericas
#   channel   = c('C3','C4','CZ',
#                 'F3','F4','F7','F8',
#                 'FP1','FP2','FZ',
#                 'O1','O2','P3','P4','PZ',
#                 'T3','T4','T5','T6',
#                 'LOG','ROG',
#                 'EMG'
#   )
  
  # nombre del archivo que contiene las epocas MOR
  setwd(e_dir)
  ar_indice = paste0('epocas_mor_',nombre,'.txt')
  indice    = scan(ar_indice)
 
  #####

  # contenedores de los datos
  RES_T   = c()
  max_epo = c()
 
  #####
  
  #if(nuevo){
    #tag  = 'EST'
    #lain = '_' 
    #}
    #if(!nuevo){
    tag  = 'RES'
    lain = ''
    #}
  
  #d_dir = paste0(data_dir,nom_len[len],'/',nom_dir) #d_# d(atos)
  d_dir = paste0(data_dir,'/',nom_dir) #d_# d(atos)
  
  setwd(d_dir)
  
  # ciclo que recorre los 22 canales
  for(ch in 1:22){
    # forma el nombre del archivo con daos
    canal  = channel[ch]
    ar_t   = paste0(tag,'_',nombre,
                    lain,canal,'_T.csv'  )
    #ar_t   = paste0('EST_',nombre,'_',canal,'_T.csv'  )
    #ar_t   = paste0('RES_',nombre,canal,'_T.csv'  )
    
    # carga los datos
    pv_t_pre = read.csv( ar_t,row.names=1 )
    pv_t     = as.numeric(unlist(pv_t_pre))
   
    # pone los datos en una matriz
    RES_T   = do.call(rbind,list(RES_T  ,pv_t ))
    max_epo = append(max_epo,length(pv_t))
  }
  
  for(ii in 1:22){
    for(jj in 1:length(RES_T[1,])){
      if(is.na(RES_T[ii,jj])){
        RES_T[ii,jj] = 0
      }
    }
  }
  
  IND_T = 1:min(max_epo)
    
  # variable auxiliar, numero de epocas totales
  n.epo = length(IND_T)
  
  # numero total de epocas y numero de epocas MOR
  print(paste0('Total : ', toString(n.epo)))
  print(paste0('  MOR : ', toString(length(indice) )))
  
  ####
  
  ####
  
  # nombre de archivo acorde a argumentos opcionales
  if(porcent){
    tag = 'porcentaje'
  }
  if(!porcent){
    tag = 'total'
  }
  
  # guardado automatico del grafico resultante
  if(grabar){
    setwd(g_dir)
    #pdf(paste0(nombre,'_',#toString(length(indice)),
    png(paste0(nombre,'_',toString(length(indice)),
               #'_',toString(n.epo),
               toString(100*p.val),
               '_difporcentaje',
               #'_',tag,
               #'.pdf'),width=12,height=6)
               '.png'),units='in',res=150,width=12,height=6)
  }
  
  ####
  ####
  ####
  
  indixe = indice
  
  #if(frecuencia==200){
  #    # 30 s  : 3 epocas en 1 bloque
  #    indice = ceiling(indice/3)
  #    indice = unique(indice)
  #    indice = sort(indice)
  #}
  
  if(frecuencia==512){
    if(len==1){
      # 60 s  : 2 epocas en 1 bloque
      indixe = ceiling(indice/2)
    }
    if(len==2){
      # 30 s  : 1 epoca por bloque
      # no se hace nada
    }
    if(len==3){
      # 15 s  : 1 epoca es 2 bloques
      indixe = c(indice*2,indice*2-1)
    }
    if(len==4){
      # 10 s  : 1 epoca es 3 bloques
      indixe = c(indice*3,indice*3-1,indice*3-2)
    }
    if(len==5){
      #  5 s  : 1 epoca es 6 bloques
      indixe = c(indice*6  ,indice*6-1,indice*6-2,
                 indice*6-3,indice*6-5)
    }
    if(len==6){
      #  2.5 s: 1 epoca es 12 bloques
      indixe = c(indice*12  ,indice*12-1,indice*12-2,
                 indice*12-3,indice*12-4,indice*12-5,
                 indice*12-6,indice*12-7,indice*12-8,
                 indice*12-9,indice*12-10,indice*12-11)
    }
  }
  if(frecuencia==200){
    if(len==1){
      # 60 s  : 6 epocas en 1 bloque
      indixe = ceiling(indice/6)
    }
    if(len==2){
      # 30 s  : 3 epocas en 1 bloque
      indixe = ceiling(indice/3)
    }
    if(len==3){
      # 15 s  : 2 epoca es 3 bloques
      indixe = c(indice*2,indice*2-1)
    }
    if(len==4){
      # 10 s  : 1 epoca es 1 bloque
      # no se hace nada
    }
    if(len==5){
      #  5 s  : 1 epoca es 2 bloques
      indixe = c(indice*2,indice*2-1)
    }
    if(len==6){
      #  2.5 s: 1 epoca es 4 bloques
      indixe = c(indice*4  ,indice*4-1,indice*4-2,
                 indice*4-3)
    }
  }
  
  indixe = unique(indixe)
  indixe = sort(indixe)
  
  ###################################################
  ###################################################
  
  #indice = indixe
  
  ###################################################
  ###################################################
  
  # nombres un poco mas sencillos
  mor   = indice
  n.mor = setdiff(1:n.epo,mor)
  
  # se cuentan las epocas estacionarias en 3 categorias
  #      tot : todas las epocas del registro
  #      mor : las epocas mor
  #     nmor : 
  
  # contenedores de resultado
  res_tot  = rep(0,22)
  res_mor  = rep(0,22)
  res_nmor = rep(0,22)
  
  # conteo de epocas estacionarias
  for(ch in 1:22){
    res_tot[ch]  = sum((RES_T[ch,]>p.val)*1)
    res_nmor[ch] = sum((RES_T[ch,n.mor]>p.val)*1)
    res_mor[ch]  = sum((RES_T[ch,mor]>p.val)*1)
  }
  
  ################################
  significados = rep(0,22)
  
  for(ch in 1:22){
    tu = prop.test(x=c(res_nmor[ch],res_mor[ch]),n=c(length(RES_T[ch,]),length(mor)))
    significados[ch] = as.numeric(tu['p.value'])
  }
  ################################
  
  # participacion relativa
  if(porcent){
    res_tot  =  res_tot/length(RES_T[1,])
    res_nmor = res_nmor/length(n.mor)
    res_mor  =  res_mor/length(mor)
  }
  
  # matriz con todos los datos
  ress = rbind(res_tot,res_nmor,res_mor)
  #ress = rbind(res_tot,res_mor)
  #ress = t(ress)
  
  # fijar el maximo del grafico
  max_r = (max(ress))
  if(escala){
    max_r = 1
  }
  
  # el mensaje cambia si es total o proporcion
  if(porcent){
    yl = 'Proporcion'
  }
  if(!porcent){
    yl = 'Total'
  }
  
#   # se grafican las cantidades
#   barplot(ress,space=c(0,.5),
#           ylim=c(0,max_r),
#           names.arg=channel,
#           col=c('black','gray','green'),
#           #col=c('black','green'),
#           border=NA,xpd=F,beside=T,
#           main=paste0(nombre_abreviado,' , *=',
#                       toString(p.val)),
#           ylab=paste0(yl,' de epocas estacionarias'))
#   
#   legend('topleft',
#          c('Total','no-MOR','MOR'),
#          fill=c('black','gray','green'),
#          #c('Total','MOR'),
#          #fill=c('black','green'),
#          bty='o',y.intersp=1,
#          xjust=1,yjust=0,
#          cex=1)
  
  MINI = floor(20*min(c(ress[1,],ress[3,])))/20
  MAXI = ceiling(20*max(c(ress[1,],ress[3,])))/20
  
  plot(ress[1,],xaxt='n',
       ylim=c(MINI,MAXI),
       xlab='',ylab='% epocas estacionarias',
       main=nombre_abreviado,
       type='l',col='red',lwd=2)
  
  lines(ress[1,],xaxt='n',
        ylim=c(MINI,MAXI),
        type='p',col='red',pch=19)
  
  lines(ress[3,],xaxt='n',
        ylim=c(MINI,MAXI),
        type='l',col='blue',lwd=2)
  lines(ress[3,],xaxt='n',
        ylim=c(MINI,MAXI),
        type='p',col='blue',pch=19)
  
  axis(1,at=1:22,labels=(channel),las=2,
       tick=T)
  
  legend('topright',
         legend=c('Total','MOR'),
         col=c('red','blue'),
         lty=1,lwd=2,cex=0.75)
  
  ####
  ####
  
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
  
  ####
  ####
  
  # guardado automatizado de los resultados
  if(grabar){
    setwd(g_dir)
    dev.off()
  }
  
  #matriz_mor = matrix(nrow=22,ncol=11)
  
  for(ch in 1:22){
    matriz_mor[sujeto,ch] = ress[3,ch]
  }
  
  for(ch in 1:22){
    matriz_nmor[sujeto,ch] = ress[2,ch]
  }
  
  for(ch in 1:22){
    matriz_tot[sujeto,ch] = ress[1,ch]
  }
  
  #row.names(matriz_mor)=channel
  #colnames(matriz_mor) = nomb_facil
  
  ####
  