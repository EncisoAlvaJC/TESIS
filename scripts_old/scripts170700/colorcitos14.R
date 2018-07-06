###################################################################################################
###################################################################################################
###################################################################################################

psr_grafico = function(nombre,
                       inicio  = c(0,0,0,0)){
  
  #################################################
  # libreria especifica para el grafico tipo matriz
  require('plotrix')
}

###################################################################################################
###################################################################################################
###################################################################################################

min_t = 4*60+20
max_t = 5*60+35

min_t = min_t*2+1
max_t = max_t*2+1
########################################
########################################

  #####

  
  
  duracionnn = c(30,10,60,2.5)
  durrr_nomb = c('30','10','60','2_5')
  
  #duracion_tal=1
  
  duracion = duracionnn[duracion_tal]
  dur_nomb = durrr_nomb[duracion_tal]
  
  # nombres de las carpetas
  #       data  directorio donde estan los datos
  #    central  directorio donde guardar los graficos, debe
  #             contener el subdirectorio con las epocas
  data_dir    = 'C:/Users/Erika/Desktop/Julio161213/scripts170506/estacionariedad_'
  central_dir = 'C:/Users/Erika/Desktop/Julio161213/scripts170506'
  
  data_dir = paste0(data_dir,dur_nomb,'s/')
  
  

  frecuencia = frecuenciasss[sujeto]
  
  #####
  #####
  #####
  
  
  # control manual, paso intermedio a la automatizacion
  total   = T               # F: solo MOR , T: todo el registro
  binario = T
  
  
  
  #p.vales = c(.1,.05,.01) # p-valores para la clasificacion
  
  p.vales = c(.05,.05,.05)
  
  #####
  #####
  #####
  
  # VARIABLES PARA RETIRAR
  escala  = F   # el significado de los colores
  
  # automatizacion de directorio datos segun sujeto
  nombre_abreviado = nomb_facil[sujeto]
  nombre           = nomb_arch[sujeto]
  nom_dir          = nomb_dir[sujeto]
  
  # directorios de trabajo, nombres abreviados
  d_dir   = paste0(data_dir,nom_dir)       # d(atos)
  e_dir   = paste0(central_dir,'/epocas3') # e(pocas)
  #e_dir   = paste0(central_dir,'/epocas_valeria') # e(pocas)
  r_dir   = central_dir                    # r(esultados)
  g_dir   = paste0(central_dir,
                   '/retoques_170608')   # g(raficos)
  
  #####
  
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
  
  tag  = 'EST'
  lain = '_' 
    
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
    
    n_total = length(pv_t)
    
    # si solo se pide MOR, toma del indice
    #if(!total){
    #  pv_t  = pv_t[indice]
    #}
   
    # pone los datos en una matriz
    RES_T   = do.call(rbind,list(RES_T  ,pv_t ))
    max_epo = append(max_epo,length(pv_t))
  }
  
  ####
  ####
  
  indiz = 0:(n_total+1)
  
  hors = floor(indiz/(epo_s_min*60))
  minu = floor((indiz -hors*(epo_s_min*60))/(epo_s_min) )
  
  tiempo = c()
  for(i in 1:(length(indiz))){
    mmm = toString(minu[i])
    if(minu[i]<10){
      mmm = paste0('0',minu[i])
    }
    tiempo = c(tiempo,paste0(toString(hors[i]),':',mmm))
  }
  
  ####
  
  paso = epo_s_min*5
  
  ###########
  ###########
  
  if(!total){
    paso = ceiling(length(indice)/50)
    IND_T = indice
    tiempo = tiempo[indice_old]
  }
  if(total){
    IND_T = 1:min(max_epo)
  }
  
  ###########
  ###########
    
  # variable auxiliar, numero de epocas totales
  n_total = length(IND_T)
  
  # numero total de epocas y numero de epocas MOR
  print(paste0('Total : ', toString(n_total)))
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
  ####
  
  # una copia de las epocas MOR, para el doble color
  RES2 = RES_T
  if(total){
    for(ii in 1:22){
      RES_T[ii,indice] = NA
    }
  }
  
  ind_menor = min_t:max_t
  len_m = length(ind_menor)
  
  RES_T_Q = matrix(nrow=22,ncol=len_m)
  RES_2_Q = matrix(nrow=22,ncol=len_m)
  
  for(ch in 1:22){
    RES_T_Q[ch,] = RES_T[ch,min_t:max_t]
    RES_2_Q[ch,] = RES2[ch,min_t:max_t]
  }
  
  RES_T = RES_T_Q
  RES2  = RES_2_Q
  
  ####
  
  # guardado automatico del grafico resultante
  if(grabar){
    if(total){
      tag = 'total'
    }
    if(!total){
      tag = 'mor'
    }
    
    setwd(g_dir)
    #pdf(paste0(nombre,#'_',toString(length(indice)),
    png(paste0(nombre,
               '_est',
    #           '.pdf'),width=12,height=6)
    #           '.png'),units='in',res=150,width=12,height=6)
                '.png'),units='in',res=150,width=12,height=4)
    #            '.png'),units='cm',res=250,width=24,height=8.5)
  }
  
  ####
  ####
  
  # esta parte grafica solo las epocas MOR
  #par(fig=c(0,.75,0,1),new=F) #para juntar los graficos
  par(new=F) #para juntar los graficos
  
  par(mar=c(2,2,2,1))
  
  # el grafico per se
  #par(mar=c(5,4,4,0))
  color2D.matplot(RES2,
                  #colores claro->oscuro ~ menor->mayor
                  cs1=c(.6745,.1922),
                  cs2=c(1    ,.5725),
                  cs3=c(.5059,0    ),
                  border=NA,axes=F,na.color=NA,
                  xlab='Tiempo (hh:mm)',ylab='',
                  main=paste0('Sujeto : ',
                              nombre_abreviado,#,nuevo
                              '  (',
                              dur_nomb,
                              ' s)'
                              #' , *=',toString(p.val)
                              )
                  )
  # esta parte grafica el resto de las epocas
  #par(fig=c(0,.75,0,1),new=T) #juntar: recordar cual es T
  par(new=T) #juntar: recordar cual es T
  color2D.matplot(RES_T,
                  cs1=c(1,0),
                  cs2=c(1,0),
                  cs3=c(1,0),
                  border=NA,axes=F,na.color=NA,
                  xlab='',ylab='',main='')
  
  # detalles de los ejes: 
  #       en el eje horizontal estan los canales,
  #       en el eje vertical los numeros de epoca
  # son duplicados, uno pone ticks y el otro etiquetas
  axis(2,at=1:22-0.5,labels=rev(channel),las=2,
       tick=F)
  axis(2,at=0:22,    labels=F,           las=2,
       tick=T)
  skip = seq(min_t,max_t+2,by=paso)
  
  axis(1,at=skip-min_t,labels=tiempo[skip],las=2,
       tick=F)
  axis(1,at=skip-min_t, labels=F,              las=2,
       tick=T)
  axis(3,labels=F,tick=T,at=c(0,n_total))
  
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