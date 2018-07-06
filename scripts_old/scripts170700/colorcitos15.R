###############################################################################
# Script para detectar estacionariedad debil en registros electrofisiologicos,
# por Enciso Alva, 2017
# Para citar y revisar instrucciones de uso, revisar documantacion anexa
#

###############################################################################
# parametros del script, ver documantacion para mas informacion
#nombre       = 'CLMN10SUE'
#etiqueta     = 'CLMN'
nombre      = 'MJNNVIGILOScCanal'
etiqueta    = 'MJNN'
dir_res_mid  = paste0(getwd(),'/res_parciales')
dir_graf     = paste0(getwd(),'/graficos')
fr_muestreo  = 512
dur_epoca    = 30

reemplazar = TRUE
grabar     = FALSE

canales      = 'PSG'
#canales     = c('C3','C4','CZ','F3','F4','F7','F8','FP1','FP2','FZ','O1','O2',
#                'P3','P4','PZ','T3','T4','T5','T6')

binario = T
p.vales = c(.05,.01,.005)
escala  = F
paso    = 5

str_hms = c(1,0,0)

#################################################
# parametros opcionales (zoom)
zoom        = FALSE
unidad_zoom = 'tiempo'
#unidad_zoom = 'epoca'

min_hms = c(-1 ,0,0)
max_hms = c(100,0,0)

min_epo = -1
max_epo = 10^12

# procesamiento parametros opcionales (zoom)
if(zoom){
  confirma_zoom = FALSE
  if(unidad_zoom[1]=='t' || unidad_zoom[1]=='T'){
    min_t = min_hms[1]*60*60 + min_hms[2]*60 + min_hms[3]
    max_t = max_hms[1]*60*60 + max_hms[2]*60 + max_hms[3]

    min_e = floor(min_t/dur_epoca)
    max_e = ceiling(max_t/dur_epoca)
    
    confirma_zoom = TRUE
  }
  if(unidad_zoom[1]=='e' || unidad_zoom[1]=='E'){
    min_t = min_epo*dur_epoca
    max_t = max_epo*dur_epoca

    min_e = min_epo
    max_e = max_epo
    
    confirma_zoom = TRUE
  }
  if(!confirma_zoom){
    warning('WARNING: Indique la unidad de tiempo para el zoom (epocas o segundos)')
  }
}

#################################################
# libreria especifica para el grafico tipo matriz
require('plotrix')

#################################################
# procesamiento parametros opcionales
if(reemplazar){
  if(canales=='10-20'){
    canales = c('C3','C4','CZ','F3','F4','F7','F8','FP1','FP2','FZ','O1','O2',
                'P3','P4','PZ','T3','T4','T5','T6')
  }
  if(canales=='PSG'){
    canales = c('C3','C4','CZ','F3','F4','F7','F8','FP1','FP2','FZ','O1','O2',
                'P3','P4','PZ','T3','T4','T5','T6','LOG','ROG','EMG')
  }
}
if(length(canales)<1){
  stop('ERROR: Lista de canales tiene longitud cero')
}

#################################################
# parametros dependientes de los datos
n_canales = length(canales)

#################################################
# optimizacion: lee el tamano de los datos el contenedor
setwd(dir_res_mid)
ch        = 1
ch_actual = canales[ch]
nom_arch  = paste0('EST_',nombre,'_',ch_actual,'_T.csv'  )
pv_t_pre  = read.csv(nom_arch,row.names=1 )
pv_t      = as.numeric(unlist(pv_t_pre))
n_epocas  = length(pv_t)



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

#################################################
# contenedores de los datos
RES_T   = matrix(nrow=n_canales,ncol=n_epocas)

#################################################
# inicio ciclo que recorre todos los canales
for(ch in 1:n_canales){
  # cargar los datos
  ch_actual = canales[ch]
  nom_arch  = paste0('EST_',nombre,'_',ch_actual,'_T.csv'  )
  pv_t_pre  = read.csv(nom_arch,row.names=1 )
  pv_t      = as.numeric(unlist(pv_t_pre))
  
  # si en algun canal se analizaron mas o menos epocas
  if(length(pv_t)!=n_epocas){
    warning('ERROR: En canal ',ch_actual,', no coincide el numero de epocas')
    if(length(pv_t)>n_epocas){
      pv_t  = pv_t[1:n_epocas]
    }else{
      pv_t2 = pv_t
      pv_t  = rep(1,n_epocas)
      pv_t[1:length(pv_t2)] = pv_t2
    }
  }
  
  # organizacion de los datos en una matriz
  RES_T[ch,] = pv_t
}
# fin ciclo que recorre canales
#################################################

#################################################
# creacion etiquetas de tiempo
ind_t  = (0:n_epocas)*(dur_epoca) + (str_hms[1]*60*60+str_hms[2]*60+str_hms[3])
ind_hh = floor(ind_t/(60*60))
ind_mm = floor( (ind_t - ind_hh*60*60)/60 )
txt_t  = character(n_epocas+1)
for(i in 1:(n_epocas+1)){
  txt_mm = toString(ind_mm[i])
  if(ind_mm[i]<10){
    txt_mm = paste0('0',ind_mm[i])
  }
  txt_t[i] = paste0(toString(ind_hh[i]),':',txt_mm)
}

pass  = paso/dur_epoca
IND_T = ind_t-1

#################################################
# tratamiento para contraste de color
if(binario){
  n_pvals = length(p.vales)
  M_RES   = matrix(0,nrow=n_canales,ncol=n_epocas)
  for(i in 1:n_pvals){
    M_RES = M_RES + 1*( RES_T>p.vales[i])
  }
  RES_T   = M_RES**(3/2)
}

#################################################
# inicio guardado automatico del grafico
if(grabar){
  setwd(dir_graf)
  #pdf(paste0(nombre,
  png(paste0(nombre,
             '_est',
  #           '.pdf'),width=12,height=6)
             '.png'),units='in',res=150,width=12,height=6)
}

# grafico principal
par(mar=c(4,3,3.5,1))
color2D.matplot(RES_T,
                #colores claro->oscuro ~ menor->mayor
                cs1=c(1,0),cs2=c(1,0),cs3=c(1,0),
                border=NA,axes=F,na.color=NA,
                xlab='Tiempo (hh:mm)',ylab='',
                main=paste0('Sujeto : ',etiqueta,
                            '  (',toString(dur_epoca),' s)'
                            )
                )
  
# los ejes
axis(2,at=1:22-0.5,labels=rev(canales),las=2,tick=F)
axis(2,at=0:22,    labels=F,           las=2,tick=T)
axis(3,labels=F,tick=T,at=c(0,n_epocas))
skip = seq(1,n_epocas+1,by=paso)
axis(1,at=skip-1,labels=txt_t[skip],las=2,tick=T)
  
# significado de los colores
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

if(grabar){
  setwd(dir_graf)
  dev.off()
}
# fin guardado automatico del grafico
#################################################


  # # nombre del archivo que contiene las epocas MOR
  # setwd(e_dir)
  # ar_indice = paste0('epocas_mor_',nombre,'.txt')
  # indice    = scan(ar_indice)
  # 
  # # numero total de epocas y numero de epocas MOR
  # print(paste0('Total : ', toString(n_total)))
  # print(paste0('  MOR : ', toString(length(indice) )))
  # 
  # 
  # cs1=c(.6745,.1922),
  # cs2=c(1    ,.5725),
  # cs3=c(.5059,0    ),
  # 
  # #par(fig=c(0,.75,0,1),new=F) #para juntar los graficos

# # CODIGO PREVIAMENTE RETIRADO
# # cuando solo se grafican epocas MOR, separa bloques
# # de epocas MOR seguidas, defectuoso
# for(i in (1:(length(RES_T[1,])-1))){
#   if( abs(IND_T[i]-(IND_T[i+1]-1)) > 2){
#     abline(v=i,col='red',lty=2)
#   }
# }