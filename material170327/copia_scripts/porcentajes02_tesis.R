  # libreria especifica para el grafico tipo matriz
  library(plotrix)
  
  #####

  # nombres de las carpetas
  #       data  directorio donde estan los datos
  #    central  directorio donde guardar los graficos, debe
  #             contener el subdirectorio con las epocas
  data_dir    = 'C:/Users/Erika/Desktop/Julio161213/scripts170308/estacionariedad_n_30s/'
  central_dir = 'C:/Users/Erika/Desktop/Julio161213/scripts170308'
  #data_dir    = '~/scripts170213/estacionariedad_recalculada/'
  #central_dir = '~/scripts170213'
  
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
  
  frecuencias = c(512,512,512,
                  512,512,
                  200,200,
                  512,
                  200,
                  512,512)
  frecuencia = frecuencias[sujeto]
  
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
  
  # VARIABLES PARA RETIRAR
  # control manual de epocas graficadas
  binario = T   # contraste acepta/rechaza estacionariedad
  nuevo   = T   # en ves de RES(ultado) usa EST(acionario)
  
  # automatizacion de directorio datos segun sujeto
  nombre_abreviado = nomb_facil[sujeto]
  nombre  = nomb_arch[sujeto]
  nom_dir = nomb_dir[sujeto]
  
  # directorios de trabajo, nombres abreviados
  d_dir   = paste0(data_dir,nom_dir)      # d(atos)
  e_dir   = paste0(central_dir,'/epocas2') # e(pocas)
  r_dir   = central_dir                   # r(esultados)
  g_dir   = paste0(central_dir,
                   '/enviar170302')# g(raficos)
  
  #####
  
  # constantes genericas
  channel   = c('C3','C4','CZ',
                'F3','F4','F7','F8',
                'FP1','FP2','FZ',
                'O1','O2','P3','P4','PZ',
                'T3','T4','T5','T6',
                'LOG','ROG',
                'EMG'
  )
  
  # nombre del archivo que contiene las epocas MOR
  setwd(e_dir)
  ar_indice = paste0('epocas_mor_',nombre,'.txt')
  indice    = scan(ar_indice)
 
  #####

  # contenedores de los datos
  RES_T   = c()
  max_epo = c()
 
  #####
  
  if(nuevo){
    tag  = 'EST'
    lain = '_' 
  }
  if(!nuevo){
    tag  = 'RES'
    lain = ''
  }
  
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
    pdf(paste0(nombre,'_',toString(length(indice)),
    #png(paste0(nombre,'_',toString(length(indice)),
               '_',toString(n.epo),
               '_',toString(100*p.val),
               '_bar',
               '_',tag,
               '.pdf'),width=12,height=6)
               #'.png'),units='in',res=150,width=12,height=6)
  }
  
  ####
  ####
  ####
  
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
  
  significados = rep(0,22)
  
  for(ch in 1:22){
    tu = prop.test(x=c(res_tot[ch],res_mor[ch]),n=c(length(RES_T[ch,]),length(mor)))
    significados[ch] = as.numeric(tu['p.value'])
  }
  
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
    yl = '%'
  }
  if(!porcent){
    yl = 'Total'
  }
  
  # se grafican las cantidades
  # barplot(ress,space=c(0,.5),
  #         ylim=c(0,max_r),
  #         names.arg=channel,
  #         col=c('black','gray','green'),
  #         #col=c('black','green'),
  #         border=NA,xpd=F,beside=T,
  #         main=paste0(nombre_abreviado,' , *=',
  #                     toString(p.val)),
  #         ylab=paste0(yl,' de epocas estacionarias'))
  # 
  # legend('topleft',
  #        c('Total','no-MOR','MOR'),
  #        fill=c('black','gray','green'),
  #        #c('Total','MOR'),
  #        #fill=c('black','green'),
  #        bty='o',y.intersp=1,
  #        xjust=1,yjust=0,
  #        cex=1)
  
  MINI = floor(min(ress[c(1,3),])*20)/20
  MAXI = ceiling(max(ress[c(1,3),])*20)/20
  
  #axis(lim=c(MINI,MAXI))
  
    plot(ress[1,],type='l',col='blue',
       main=paste0(nombre_abreviado,' , *=',
                   toString(p.val)),
       ylab=paste0(yl,' epocas estacionarias'),
       xlab='',ylim=c(MINI,MAXI),
       xaxt='n',
       lwd=1.5,pch=16)
    
    for(i in 1:22){
      abline(v=i,col='gray90')
    }
    
    lines(ress[1,],type='o',col='blue',
         ylim=c(MINI,MAXI),
         xaxt='n',
         lwd=1.5,pch=16)
    
    lines(ress[3,],type='l',col='red',
       xlab='',ylim=c(MINI,MAXI),
       lwd=1.5)
    
    lines(ress[3,],type='o',col='red',
          xlab='',ylim=c(MINI,MAXI),
          lwd=1.5,pch=16)
    
    axis(1,at=1:22,labels=channel,las=2,
         tick=T)
    
    # equis = (significados<.1)
    # ch_lin = 1:22
    # lines(ch_lin[equis],rep(MAXI,sum(equis*1)),
    #       type='o',pch='*',lwd=0,cex=2)
    
    strst=(MAXI-MINI)/40
    
    ch_lin = 1:22
    equis = (significados<.1)
    lines(ch_lin[equis],rep(MAXI        ,sum(equis*1)),
          type='o',pch='*',lwd=0,cex=2)
    equis = (significados<.05)
    lines(ch_lin[equis],rep(MAXI-  strst,sum(equis*1)),
          type='o',pch='*',lwd=0,cex=2)
    equis = (significados<.01)
    lines(ch_lin[equis],rep(MAXI-2*strst,sum(equis*1)),
          type='o',pch='*',lwd=0,cex=2)
    
     # axis(2,at=1:22-.5,    labels=F,      las=2,
     #      tick=T)
     # skip = seq(0,n.epo-1,by=paso)
     # axis(1,at=skip+.5,labels=IND_T[skip+1]-1,las=2,
     #      tick=F)
     # axis(1,at=skip-0, labels=F,                las=2,
     #      tick=T)
  
  # barplot(ress,space=c(0,.5),
  #         ylim=c(0,max_r),
  #         names.arg=channel,
  #         col=c('black','gray','green'),
  #         #col=c('black','green'),
  #         border=NA,xpd=F,beside=T,
  #         main=paste0(nombre_abreviado,' , *=',
  #                     toString(p.val)),
  #         ylab=paste0(yl,' de epocas estacionarias'))
  
  # legend('topleft',
  #        c('Total','no-MOR','MOR'),
  #        fill=c('black','gray','green'),
  #        #c('Total','MOR'),
  #        #fill=c('black','green'),
  #        bty='o',y.intersp=1,
  #        xjust=1,yjust=0,
  #        cex=1)
  
  ####
  ####
  
  # guardado automatizado de los resultados
  if(grabar){
    setwd(g_dir)
    dev.off()
  }
  
  ####