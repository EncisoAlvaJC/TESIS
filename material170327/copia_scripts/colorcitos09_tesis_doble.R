  # libreria especifica para el grafico tipo matriz
  library(plotrix)
  
  #####

  # nombres de las carpetas
  #       data  directorio donde estan los datos
  #    central  directorio donde guardar los graficos, debe
  #             contener el subdirectorio con las epocas
  data_dir    = 'C:/Users/Erika/Desktop/Julio161213/scripts170308/estacionariedad_n_60s/'
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
  #####
  #####
  #####
  
  # control manual, paso intermedio a la automatizacion
  #sujeto  = 9     # numero de sujeto, en orden alfabetico 
  paso    = 50    # salto en las etiquetas de n. epoca
  #p.val   = .01   # p-valor de rechazo para la hipotesis
  #grabar  = F     # guardado automatico de los graficos
  total   = T     # F: solo MOR , T: todo el registro 
  nuevo   = T     # en ves de RES(ultado) usa EST(acionario)
  
  bis     = F     # para los datos que tienen mor corregidos
  
  p.vales = c(.1,.05,.01)
  
  #####
  #####
  #####
  
  # VARIABLES PARA RETIRAR
  # control manual de epocas graficadas
  binario = T   # contraste acepta/rechaza estacionariedad
  escala  = F   # el significado de los colores
  
  # automatizacion de directorio datos segun sujeto
  nombre_abreviado = nomb_facil[sujeto]
  nombre  = nomb_arch[sujeto]
  nom_dir = nomb_dir[sujeto]
  
  # directorios de trabajo, nombres abreviados
  d_dir   = paste0(data_dir,nom_dir)      # d(atos)
  e_dir   = paste0(central_dir,'/epocas2') # e(pocas)
  r_dir   = central_dir                   # r(esultados)
  g_dir   = paste0(central_dir,
                   '/graf_tiempo_doble')             # g(raficos)
  
  #####
  
  #if(bilateral){
  #  # constantes genericas
  #  channel   = c('C3','CZ','C4',
  #                'F3','F4','F7','F8',
  #                'FP1','FP2','FZ',
  #                'O1','O2','P3','P4','PZ',
  #                'T3','T4','T6','T5',
  ##                'LOG','ROG',
  #                'EMG'
  #  )
  #}
  #if(!bilateral){
    # constantes genericas
    channel   = c('C3','C4','CZ',
                  'F3','F4','F7','F8',
                  'FP1','FP2','FZ',
                  'O1','O2','P3','P4','PZ',
                  'T3','T4','T5','T6',
                  'LOG','ROG',
                  'EMG'
    )
  #}
  
  # nombre del archivo que contiene las epocas MOR
  setwd(e_dir)
  if(bis){
    ar_indice = paste0('epocas_mor_',nombre,'_2.txt')
  }
  if(!bis){
    ar_indice = paste0('epocas_mor_',nombre,'.txt')
  }
  indice    = scan(ar_indice)
 
  indice = ceiling(indice/2)
  indice = unique(indice)
  indice = sort(indice)
  
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
   
    # carga los datos
    pv_t_pre = read.csv( ar_t,row.names=1 )
    pv_t     = as.numeric(unlist(pv_t_pre))
    
    #print(length(pv_t))
    
    n_total = length(pv_t)
    
    # si solo se pide MOR, toma del indice
    if(!total){
      pv_t  = pv_t[indice]
   
      # elimina los NA, para registros incompletos
      #ind1  = 1:length(pv_t)
      #err   = is.na(pv_t)
      #ind2  = setdiff(ind1,ind1[err])
      #pv_t  = pv_t[ind2]
    }
   
    # pone los datos en una matriz
    RES_T   = do.call(rbind,list(RES_T  ,pv_t ))
    max_epo = append(max_epo,length(pv_t))
  }
  
  ####
  ####
  
  indiz = 1:(n_total+2)
  if(frecuencia == 512){
    minu = floor((indiz -floor(indiz/(60))*(60))/(1) )
    hors = floor(indiz/(60))
    
    tiempo = c()
    for(i in 1:(length(indiz))){
      mmm = toString(minu[i])
      if(minu[i]<10){
        mmm = paste0('0',minu[i])
      }
      tiempo = c(tiempo,paste0(toString(hors[i]),':',mmm))
    }
    
    ####
    
    paso = 10
  }
  if(frecuencia == 200){
    minu = floor((indiz -floor(indiz/(3*60))*(3*60))/(3*1) )
    hors = floor(indiz/(3*60))
    
    tiempo = c()
    for(i in 1:(length(indiz))){
      mmm = toString(minu[i])
      if(minu[i]<10){
        mmm = paste0('0',minu[i])
      }
      tiempo = c(tiempo,paste0(toString(hors[i]),':',mmm))
    }
    
    ####
    
    paso = 3*10
  }
  
  if(!total){
    paso = ceiling(length(indice)/50)
  }
  
  ####
  ####
  
  if(total){
    IND_T = 1:min(max_epo)
  }
  if(!total){
    IND_T = indice
    tiempo = tiempo[indice]
  }
  
  ####
  ####
    
  # variable auxiliar, numero de epocas totales
  n.epo = length(IND_T)
  
  # numero total de epocas y numero de epocas MOR
  print(paste0('Total : ', toString(n.epo)))
  print(paste0('  MOR : ', toString(length(indice) )))

  ####
  
  # VARIABLE PARA RETIRAR
  # el contraste aceptacion/rechazo de la hipotesis nula
  
  if(binario){
    RES_T = (1*( RES_T > p.vales[1] ) + 
             1*( RES_T > p.vales[2] ) +
             1*( RES_T > p.vales[3] )
            )**(3/2)
  }
  
  RES_T = RES_T[rev(1:22),]
  
  ####
  
  # una copia de las epocas MOR, para el doble color
  RES2 = RES_T
  if(total){
    for(ii in 1:22){
      RES_T[ii,indice] = NA
    }
  }
  
  ####
  
  # guardado automatico del grafico resultante
  if(grabar){
    if(total){
      tag = 'total'
    }
    if(!total){
      tag = 'mor'
    }
    
    if(bis){
      nuevo = '_2'
    }
    if(!bis){
      nuevo = ''
    }
    
    setwd(g_dir)
    pdf(paste0(nombre,'_',toString(length(indice)),
    #png(paste0(nombre,
               nuevo,
               '_mor',toString(length(indice)),
               '_tot',toString(n.epo),
    #           '_p',toString(100*p.val),
               '_est_',
               tag,
               '_dobleepoca',
               '.pdf'),width=12,height=6)
               #'.png'),units='in',res=150,width=12,height=6)
  }
  
  ####
  
  RES_T = RES_T[rev(1:22),]
  RES2  = RES2[rev(1:22),]
  
  ####
  
  # esta parte grafica solo las epocas MOR
  #par(fig=c(0,.75,0,1),new=F) #para juntar los graficos
  par(new=F) #para juntar los graficos
  
  # el grafico per se
  #par(mar=c(5,4,4,0))
  
  if(bis){
    nuevo = ' (nuevo)'
  }
  if(!bis){
    nuevo = ''
  }
  
  color2D.matplot(RES2,
                  #colores claro->oscuro ~ menor->mayor
                  cs1=c(.6745,.1922),
                  cs2=c(1    ,.5725),
                  cs3=c(.5059,0    ),
                  border=NA,axes=F,na.color=NA,
                  xlab='Tiempo (hh:mm)',ylab='',
                  main=paste0(nombre_abreviado,nuevo,
                              ' (doble epoca)'
                              #' , *=',toString(p.val)
                              ))
  # esta parte grafica el resto de las epocas
  #par(fig=c(0,.75,0,1),new=T) #juntar: recordar cual es T
  par(new=T) #juntar: recordar cual es T
  color2D.matplot(RES_T,
                  cs1=c(1,0),
                  cs2=c(1,0),
                  cs3=c(1,0),
                  border=NA,axes=F,na.color=NA,
                  xlab='',ylab='',main='')
  
  
  
  #paso = ceiling(n.epo/50)
  
  
  # detalles de los ejes: 
  #       en el eje horizontal estan los canales,
  #       en el eje vertical los numeros de epoca
  # son duplicados, uno pone ticks y el otro etiquetas
  axis(2,at=1:22-0.5,labels=rev(channel),las=2,
       tick=F)
  axis(2,at=0:22,    labels=F,           las=2,
       tick=T)
  skip = seq(1,n.epo+2,by=paso)
  #axis(1,at=skip+.5,labels=IND_T[skip+1]-1,las=2,
  #     tick=F)
  axis(1,at=skip-1,labels=tiempo[skip],las=2,
       tick=F)
  axis(1,at=skip-1, labels=F,              las=2,
       tick=T)
  axis(3,labels=F,tick=T,at=c(0,n.epo))
  
  # CODIGO PREVIAMENTE RETIRADO
  # cuando solo se grafican epocas MOR, separa bloques
  # de epocas MOR seguidas, defectuoso
  for(i in (1:(length(RES_T[1,])-1))){
    if( abs(IND_T[i]-(IND_T[i+1]-1)) > 2){
      abline(v=i,col='red',lty=2)
    }
  }
  
  # cuadro con el significado de los colores
  if(escala){
    legend('topright',
           c('MOR','No-estacionario','Estacionario'),
           fill=c( rgb(.6745,1,.5059) , 
                   #rgb(.1922,.5725,0),
                  'white','black'),
           bty='o',y.intersp=0.5,
           xjust=1,yjust=0,#text.width=1,
           cex=0.65)
  }
  
  ####
  
  # guardado automatizado de los resultados
  if(grabar){
    setwd(g_dir)
    dev.off()
  }
  
  ####