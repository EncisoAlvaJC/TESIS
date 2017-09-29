#################################################
# parametros dependientes de los datos
n_canales = length(canales)
ventana   = dur_epoca*fr_muestreo

#################################################
# procesamiento parametros opcionales (ajuste)
if(unidad_par_t =='tiempo'){
  ini_t   = ajuste_ini_hms[1]*60*60
  +ajuste_ini_hms[2]*60
  +ajuste_ini_hms[3]
  ini_epo = ini_t/dur_epoca
  ini_pt  = floor(ini_t*fr_muestreo)
}
if(unidad_par_t =='puntos'){
  ini_epo = ajuste_ini_epo
  ini_t   = ini_epo*dur_epoca
  ini_pt  = ini_epo*ventana
}
str_t   = 0
str_epo = 1
str_pt  = 1

# ini : cuando inicia el archivo, util si es un fragmento
# str : cuando inicia el zoom, evita la confusion provocada por ini

min_e = 1

# procesamiento parametros opcionales (zoom)
#if(zoom){
#  confirma_zoom = FALSE
#  if(unidad_par_t == 'tiempo'){
#    min_t  = min_hms[1]*60*60 + min_hms[2]*60 + min_hms[3] -ini_t
#    max_t  = max_hms[1]*60*60 + max_hms[2]*60 + max_hms[3] -ini_t
#    
#    min_e  = floor((min_t+ini_t)/dur_epoca -ini_epo)
#    max_e  = ceiling((max_t+ini_t)/dur_epoca -ini_epo)
#    
#    confirma_zoom = TRUE
#  }
#  if(unidad_par_t == 'puntos'){
#    min_t = min_epo*dur_epoca -ini_t
#    max_t = max_epo*dur_epoca -ini_t
#    
#    min_e = floor((min_epo+ini_t) -ini_epo)
#    max_e = ceiling((max_epo+ini_t) -ini_epo)
#
#    confirma_zoom = TRUE
#  }
#  if(!confirma_zoom){
#    warning('WARNING: Indique unidad de tiempo para zoom (epocas o segundos)')
#  }
#  min_pt = floor(min_e*ventana)
#  max_pt = ceiling(max_e*ventana)
#  
#  str_t   = max(min_t, 0)
#  str_epo = max(min_e, 1)
#  str_pt  = max(min_pt,1)
#}

#################################################
# optimizacion: lee el tamano de los datos el contenedor
setwd(dir_res_mid)
ch            = 1
ch_actual     = canales[ch]
nom_arch      = paste0('EST_',nombre,'_',ch_actual,
                       '_T_',toString(dur_epoca),'.txt')
pv_t          = scan(nom_arch)
pv_t          = as.numeric(t(pv_t))

factor_escala = dur_epoca/30
n_epocas      = length(pv_t)
max_e         = n_epocas

# ajustes en el tiempo
#if(zoom){
#  end_t    = min(max_t  , n_epocas*dur_epoca)
#  end_epo  = min(max_epo, n_epocas)
#  end_pt   = min(max_pt ,n_epocas)
#  
#  ini_t    = ini_t   + str_t
#  ini_epo  = ini_epo + str_epo
#  ini_pt   = ini_pt  + str_pt
#  n_epocas = length(str_epo:end_epo)
#} 
# end : cuando termina el zoom, evita la confusion provocada por ini

#################################################
# contenedores de los datos
RES_T   = matrix(nrow=n_canales,ncol=n_epocas)
RES_TIR = matrix(nrow=n_canales,ncol=n_epocas)

#################################################
# inicio ciclo que recorre todos los canales
for(ch in 1:n_canales){
  # cargar los datos
  ch_actual = canales[ch]
  nom_arch  = paste0('EST_',nombre,'_',ch_actual,
                     '_T_'  ,toString(dur_epoca),'.txt')
  pv_t      = scan(nom_arch)
  pv_t      = as.numeric(t(pv_t))
  
  nom_arch  = paste0('EST_',nombre,'_',ch_actual,
                     '_TIR_',toString(dur_epoca),'.txt')
  pv_tir    = scan(nom_arch)
  pv_tir    = as.numeric(t(pv_tir))
  
  #if(zoom){
  #  pv_t    =   pv_t[min_epo:max_epo]
  #  pv_tir  = pv_tir[min_epo:max_epo]
  #}
  
  # organizacion de los datos en una matriz
  RES_T[ch,]   = pv_t
  RES_TIR[ch,] = pv_tir
}
# fin ciclo que recorre canales
#################################################

#################################################
# creacion etiquetas de tiempo
#ind_t  = (0:(n_epocas))*(dur_epoca) + ini_t
#ind_hh = floor(ind_t/(60*60))
#ind_mm = floor( (ind_t - ind_hh*60*60)/60 )
#ind_ss = floor(  ind_t - ind_hh*60*60 - ind_mm*60 )
#txt_t  = character(n_epocas+1)
#for(i in 1:(n_epocas+1)){
#  txt_mm = toString(ind_mm[i])
#  if(ind_mm[i]<10){
#    txt_mm = paste0('0',ind_mm[i])
#  }
#txt_ss = toString(ind_ss[i])
#  if(ind_ss[i]<10){
#    txt_ss = paste0('0',ind_ss[i])
#  }
#  #txt_t[i] = paste0(toString(ind_hh[i]),':',txt_mm,':',txt_ss)
#  txt_t[i] = paste0(toString(ind_hh[i]),':',txt_mm)
#}

#etiqueta_epocas = character(max_epo-min_epo)
#s = seq(min_epo,max_epo)
#for(i in 1:length(s)){
#  etiqueta_epocas[i] = toString(s[i])
#}

pass  = paso/dur_epoca
#IND_T = ind_t-1

#################################################
# tratamiento para contraste de color
if(binario){
  if(FALSE){
    n_pvals = length(p.vales)
    M_RES   = matrix(0,nrow=n_canales,ncol=n_epocas)
    for(i in 1:n_pvals){
      M_RES = M_RES + 1*( RES_T>p.vales[i])
    }
    RES_T   = (M_RES/length(p.vales))**3
  }else{
    M_RES1 = 1*( RES_TIR<.05 )
    M_RES2 = 1*( RES_T  <.05 )
    M_RES = pmin(M_RES1,M_RES2)
    RES_T = (-M_RES+1)*.95 + .025
  }
  
}

#################################################
# inicio guardado automatico del grafico
#k = 1.6
#k = 1.5
#if(grabar){
#  setwd(dir_graf)
#  #pdf(
#  png(
#    paste0(nombre,'_est_',toString(dur_epoca),
#           #'.pdf'),width=5.941*k,height=1*k)
#           '.png'),units='in',res=300,width=5.941*k,height=1*k)
#}

colorgram(z=t(RES_T[rev(1:n_canales),]),outlier='black',bty='n',axes=F,
          #xlab='Tiempo (hh:mm)',ylab='',
          #xlab='Num. de epoca',
          xlab='',
          #ylab='',
          ylab=paste0(toString(dur_epoca),'s'),las=2,cex.lab=2,font.lab=2,
          colFn=grayscale,
          #zlab=paste0('Sujeto : ',etiqueta,
          #            '  | Estacionariedad'),
          zlab='',
          breaks=seq(0,1,by=.05),
          #key=vkey,key.args=c(skip=10,stretch=.4*k)
          key=0
)

# los ejes
canales_par = rep('',n_canales)
canales_non = rep('',n_canales)
for(i in 1:n_canales){
  if(i-2*floor(i/2)==0){
    canales_par[i] = canales[i]
  }else{
    canales_non[i] = canales[i]
  }
}

#axis(2,at=1:n_canales,labels=rev(canales),las=2,tick=F)
axis(2,at=1:n_canales,labels=rev(canales_par),las=2,tick=F,cex.axis=.8)
axis(2,at=0:n_canales+.5,    labels=F,           las=2,tick=T)
axis(4,at=1:n_canales,labels=rev(canales_non),las=2,tick=F,cex.axis=.8)
axis(4,at=0:n_canales+.5,    labels=F,           las=2,tick=T)

axis(3,labels=F,tick=T,at=c(0,n_epocas)+.5)
axis(4,at=c(0,n_canales)+.5,    labels=F,           las=2,tick=T)

# PARCHE
#tik = seq(0,length(etiqueta_epocas))
#axis(1,at=tik+.5,labels=F,las=2,tick=T)
#tik = (seq(1,length(etiqueta_epocas))-0.5)
#axis(1,at=tik+.5,labels=etiqueta_epocas,las=1,tick=F)

axis(1,labels=F,tick=T,at=c(0,n_epocas)+.5)
skip = seq(1,n_epocas+1,by=paso/factor_escala)+.5
#axis(1,at=skip-1,labels=txt_t[skip],las=2,tick=T)
#axis(3,at=skip-1,labels=txt_t[skip],las=2,tick=T)
axis(1,at=skip-1,labels=F,las=2,tick=T)
axis(3,at=skip-1,labels=F,las=2,tick=T)

#if(grabar){
#  setwd(dir_graf)
#  dev.off()
#}
# fin guardado automatico del grafico
#################################################

setwd(dir_actual)
