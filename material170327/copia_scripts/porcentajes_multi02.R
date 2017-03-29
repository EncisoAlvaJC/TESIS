  # libreria especifica para el grafico tipo matriz
  library(plotrix)
  
  #####
  
  # nombres de las carpetas
  #       data  directorio donde estan los datos
  #    central  directorio donde guardar los graficos, debe
  #             contener el subdirectorio con las epocas
  data_dir    = 'C:/Users/Erika/Desktop/Julio161213/scripts170308/estacionariedad_n_'
  central_dir = 'C:/Users/Erika/Desktop/Julio161213/scripts170308'
  
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
  
  # nombres de los archivos con epocas de diferentes longitudes
  nom_len = c('60s','30s','15s','10s','05s','02_5s')
  
  #####
  #####
  #####
  
  # control manual, paso intermedio a la automatizacion
  #sujeto  = 1     # numero de sujeto, en orden alfabetico 
  grabar  = F     # guardado automatico de los graficos
  porcent = T     # cantidad total o proporcional
  escala  = F     # el porcentaje se grafica entre 0 y 1
  
  #####
  #####
  #####
  
  # VARIABLES PARA RETIRAR
  # control manual de epocas graficadas
  #binario = T   # contraste acepta/rechaza estacionariedad
  #nuevo   = T   # en vez de RES(ultado) usa EST(acionario)
  
  # automatizacion de directorio datos segun sujeto
  nombre_abreviado = nomb_facil[sujeto]
  nombre  = nomb_arch[sujeto]
  nom_dir = nomb_dir[sujeto]
  
  # directorios de trabajo, nombres abreviados
  #d_dir   = paste0(data_dir,nom_dir)      # d(atos)
  e_dir   = paste0(central_dir,'/epocas2') # e(pocas)
  r_dir   = central_dir                   # r(esultados)
  g_dir   = paste0(central_dir,
                   '/test')# g(raficos)
  
  #####
  
  # constantes genericas
#   channel   = c('C3','C4','CZ','EMG',
#                 'F3','F4','F7','F8',
#                 'FP1','FP2','FZ','LOG',
#                 'O1','O2','P3','P4','PZ',
#                 'ROG','T3','T4','T5','T6'
#                 )
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
  
  # para guardar resultados
  PORCENTAJES_TOT_1 = c()
  PORCENTAJES_TOT_2 = c()
  PORCENTAJES_TOT_3 = c()
  
  PORCENTAJES_MOR_1 = c()
  PORCENTAJES_MOR_2 = c()
  PORCENTAJES_MOR_3 = c()
  
  PORCENTAJES_NMOR_1 = c()
  PORCENTAJES_NMOR_2 = c()
  PORCENTAJES_NMOR_3 = c()
 
  #####
  
  # nombres de los archivos con epocas de diferentes longitudes
  #nom_len = c('60s','30s','15s','10s','05s','02_5s')
  
  # ciclo sobre epocas de diferente longitud
  for(len in 1:6){
    d_dir = paste0(data_dir,nom_len[len],'/',nom_dir) # d(atos)
    
    # contenedores de los datos
    RES_T   = c()
    max_epo = c()
    
    #####
   
    #if(nuevo){
      tagg = 'EST_'
      sep = '_'
    #}
    #if(!nuevo){
    #  tagg = 'RES_'
    #  sep = ''
    #}
     
    setwd(d_dir)
  
    # ciclo que recorre los 22 canales
    for(ch in 1:22){
      # forma el nombre del archivo con daos
      canal  = channel[ch]
      ar_t   = paste0(tagg,nombre,sep,canal,'_T.csv'  )
   
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
    #print(paste0('Total : ', toString(n.epo)))
    #print(paste0('  MOR : ', toString(length(indice) )))
  
    ####
    
    # nombre de archivo acorde a argumentos opcionales
    if(porcent){
      tag = 'porcentaje'
    }
    if(!porcent){
      tag = 'total'
    }
  
    ####
    ####
    ####
  
    # SE TOMAN EN CUENTA LAS EPOCAS
    
    indixe = indice
    
    # nombres de los archivos con epocas de diferentes longitudes
    nom_len = c('60s','30s','15s','10s','05s','02_5s')
    
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
    
    # nombres un poco mas sencillos
    mor   = indixe
    n.mor = setdiff(1:n.epo,mor)
  
    # se cuentan las epocas estacionarias en 3 categorias
    #      tot : todas las epocas del registro
    #      mor : las epocas mor
    #     nmor : 
    
    ###############
    ###############
      # contenedores de resultado
      res_tot  = rep(0,22)
      res_mor  = rep(0,22)
      res_nmor = rep(0,22)
  
      # conteo de epocas estacionarias
      for(ch in 1:22){
        res_tot[ch]  = sum((RES_T[ch,]<0.01)*1)
        res_nmor[ch] = sum((RES_T[ch,n.mor]<0.01)*1)
        res_mor[ch]  = sum((RES_T[ch,mor]<0.01)*1)
      }
  
      # participacion relativa
      if(porcent){
        res_tot  =  res_tot/length(RES_T[1,])
        res_nmor = res_nmor/length(n.mor)
        res_mor  =  res_mor/length(mor)
      }
  
      PORCENTAJES_TOT_1  = rbind(PORCENTAJES_TOT_1,res_tot)
      PORCENTAJES_MOR_1  = rbind(PORCENTAJES_MOR_1,res_mor)
      PORCENTAJES_NMOR_1 = rbind(PORCENTAJES_NMOR_1,res_nmor)
    ###############
    ###############
      
    ###############
    ###############
      # contenedores de resultado
      res_tot  = rep(0,22)
      res_mor  = rep(0,22)
      res_nmor = rep(0,22)
      
      # conteo de epocas estacionarias
      for(ch in 1:22){
        res_tot[ch]  = sum((RES_T[ch,]<0.05)*1)
        res_nmor[ch] = sum((RES_T[ch,n.mor]<0.05)*1)
        res_mor[ch]  = sum((RES_T[ch,mor]<0.05)*1)
      }
      
      # participacion relativa
      if(porcent){
        res_tot  =  res_tot/length(RES_T[1,])
        res_nmor = res_nmor/length(n.mor)
        res_mor  =  res_mor/length(mor)
      }
      
      PORCENTAJES_TOT_2  = rbind(PORCENTAJES_TOT_2,res_tot)
      PORCENTAJES_MOR_2  = rbind(PORCENTAJES_MOR_2,res_mor)
      PORCENTAJES_NMOR_2 = rbind(PORCENTAJES_NMOR_2,res_nmor)
      ###############
      ###############
      
      ###############
      ###############
      # contenedores de resultado
      res_tot  = rep(0,22)
      res_mor  = rep(0,22)
      res_nmor = rep(0,22)
      
      # conteo de epocas estacionarias
      for(ch in 1:22){
        res_tot[ch]  = sum((RES_T[ch,]<0.1)*1)
        res_nmor[ch] = sum((RES_T[ch,n.mor]<0.1)*1)
        res_mor[ch]  = sum((RES_T[ch,mor]<0.1)*1)
      }
      
      # participacion relativa
      if(porcent){
        res_tot  =  res_tot/length(RES_T[1,])
        res_nmor = res_nmor/length(n.mor)
        res_mor  =  res_mor/length(mor)
      }
      
      PORCENTAJES_TOT_3  = rbind(PORCENTAJES_TOT_3,res_tot)
      PORCENTAJES_MOR_3  = rbind(PORCENTAJES_MOR_3,res_mor)
      PORCENTAJES_NMOR_3 = rbind(PORCENTAJES_NMOR_3,res_nmor)
      ###############
      ###############
  }
  
  ##############################
  ##############################
  
  # para una mejor visualizacion de los incrementos
  PORCENTAJES_TOT_4 = rep(1,22)
  PORCENTAJES_TOT_4 = PORCENTAJES_TOT_4 - PORCENTAJES_TOT_3 
  PORCENTAJES_TOT_3 = PORCENTAJES_TOT_3 - PORCENTAJES_TOT_2
  PORCENTAJES_TOT_2 = PORCENTAJES_TOT_2 - PORCENTAJES_TOT_1
  
  PORCENTAJES_MOR_4 = rep(1,22)
  PORCENTAJES_MOR_4 = PORCENTAJES_MOR_4 - PORCENTAJES_MOR_3 
  PORCENTAJES_MOR_3 = PORCENTAJES_MOR_3 - PORCENTAJES_MOR_2
  PORCENTAJES_MOR_2 = PORCENTAJES_MOR_2 - PORCENTAJES_MOR_1
  
  PORCENTAJES_NMOR_4 = rep(1,22)
  PORCENTAJES_NMOR_4 = PORCENTAJES_NMOR_4 - PORCENTAJES_NMOR_3 
  PORCENTAJES_NMOR_3 = PORCENTAJES_NMOR_3 - PORCENTAJES_NMOR_2
  PORCENTAJES_NMOR_2 = PORCENTAJES_NMOR_2 - PORCENTAJES_NMOR_1
  
  # matriz con todos los datos
  MATRIZ_TOT = matrix(nrow=4,ncol=22*7)
  for(i in 0:5){
    for(j in 0:21){
      MATRIZ_TOT[1,j*7+i+1] = PORCENTAJES_TOT_4[i+1,j+1]
      MATRIZ_TOT[2,j*7+i+1] = PORCENTAJES_TOT_3[i+1,j+1]
      MATRIZ_TOT[3,j*7+i+1] = PORCENTAJES_TOT_2[i+1,j+1]
      MATRIZ_TOT[4,j*7+i+1] = PORCENTAJES_TOT_1[i+1,j+1]
    }
    for(k in 1:4){
      MATRIZ_TOT[k,7*k] = 0
    }
  }
  
  MATRIZ_MOR = matrix(nrow=4,ncol=22*7)
  for(i in 0:5){
    for(j in 0:21){
      MATRIZ_MOR[1,j*7+i+1] = PORCENTAJES_MOR_4[i+1,j+1]
      MATRIZ_MOR[2,j*7+i+1] = PORCENTAJES_MOR_3[i+1,j+1]
      MATRIZ_MOR[3,j*7+i+1] = PORCENTAJES_MOR_2[i+1,j+1]
      MATRIZ_MOR[4,j*7+i+1] = PORCENTAJES_MOR_1[i+1,j+1]
    }
    for(k in 1:4){
      MATRIZ_MOR[k,7*k] = 0
    }
  }
  
  MATRIZ_NMOR = matrix(nrow=4,ncol=22*7)
  for(i in 0:5){
    for(j in 0:21){
      MATRIZ_NMOR[1,j*7+i+1] = PORCENTAJES_NMOR_4[i+1,j+1]
      MATRIZ_NMOR[2,j*7+i+1] = PORCENTAJES_NMOR_3[i+1,j+1]
      MATRIZ_NMOR[3,j*7+i+1] = PORCENTAJES_NMOR_2[i+1,j+1]
      MATRIZ_NMOR[4,j*7+i+1] = PORCENTAJES_NMOR_1[i+1,j+1]
    }
    for(k in 1:4){
      MATRIZ_NMOR[k,7*k] = 0
    }
  }
  
  # guardado automatico del grafico resultante
  #   if(grabar){
  #     setwd(g_dir)
  #     #pdf(paste0(nombre,'_',toString(length(indice)),
  #     png(paste0(nombre,'_',toString(length(indice)),
  #                '_',toString(n.epo),
  #                '_',toString(100*p.val),
  #                '_bar',
  #                '_',tag,
  #                #'.pdf'),width=12,height=6)
  #                '.png'),units='in',res=150,width=12,height=6)
  #   }
  
  # fijar el maximo del grafico
  #max_r = (max(PORCENTAJES_TOT_1))
  #if(escala){
  #  max_r = 1
  #}
  
  # el mensaje cambia si es total o proporcion
  if(porcent){
    yl = '%'
  }
  if(!porcent){
    yl = 'Total'
  }

  ################
  ################
    
#   # se grafican las cantidades
#   barplot(MATRIZ_TOT,
#           #space=c(0,.5),
#           space=0,
#           #ylim=c(0,max_r),
#           #names.arg=channel,
#           col=c('black','gray30','gray60','white'),
#           #col=c('black','gray','green'),
#           #border=NA,
#           border='black',
#           xpd=F,
#           #beside=T,
#           #main=paste0(nombre_abreviado,' , *=',
#           #            toString(p.val_1)),
#           main=paste0(nombre_abreviado,' , total'),
#           ylab=paste0(yl,' epocas estacionarias')
#           )
#   legend('topright',
#          c('No-est. *=.01',
#            'No-est. *=.05',
#            'No-est. *=.1',
#            'Estacionario'),
#          #fill=c('black','gray','green'),
#          fill=c('black','gray30','gray60','white'),
#          bty='o',#y.intersp=0.5,
#          xjust=1,yjust=0,
#          cex=1
#          )
   
   ################
   ################
   
   # se grafican las cantidades
   barplot(MATRIZ_MOR,
          #space=c(0,.5),
          space=0,
          #ylim=c(0,max_r),
          #names.arg=channel,
          col=c('black','gray30','gray60','white'),
          #col=c('black','gray','green'),
          #border=NA,
          border='black',
          xpd=F,
          #beside=T,
          #main=paste0(nombre_abreviado,' , *=',
          #            toString(p.val_1)),
          main=paste0(nombre_abreviado,' , MOR'),
          ylab=paste0(yl,' epocas estacionarias')
   )
   legend('topright',
         c('No-est. *=.01',
           'No-est. *=.05',
           'No-est. *=.1',
           'Estacionario'),
         #fill=c('black','gray','green'),
         fill=c('black','gray30','gray60','white'),
         bty='o',#y.intersp=0.5,
         xjust=1,yjust=0,
         cex=1
   )
   
   ################
   ################
    
#    # se grafican las cantidades
#    barplot(MATRIZ_NMOR,
#            #space=c(0,.5),
#            space=0,
#            #ylim=c(0,max_r),
#            #names.arg=channel,
#            col=c('black','gray30','gray60','white'),
#            #col=c('black','gray','green'),
#            #border=NA,
#            border='black',
#            xpd=F,
#            #beside=T,
#            #main=paste0(nombre_abreviado,' , *=',
#            #            toString(p.val_1)),
#            main=paste0(nombre_abreviado,' , no-MOR'),
#            ylab=paste0(yl,' epocas estacionarias')
#    )
#    legend('topright',
#           c('No-est. *=.01',
#             'No-est. *=.05',
#             'No-est. *=.1',
#             'Estacionario'),
#           #fill=c('black','gray','green'),
#           fill=c('black','gray30','gray60','white'),
#           bty='o',#y.intersp=0.5,
#           xjust=1,yjust=0,
#           cex=1
#    )
   
   ################
   ################
   
#   # guardado automatizado de los resultados
#   if(grabar){
#     setwd(g_dir)
#     dev.off()
#   }
  
  ####