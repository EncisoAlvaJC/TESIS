###############################################################################
# Script para detectar estacionariedad debil en registros electrofisiologicos,
# por Enciso Alva, 2017
# Para citar y revisar instrucciones de uso, revisar documantacion anexa
#
# Alejandra Rosales-Lagarde, Erika Rodríguez-Torres,  Julio Enciso-Alva, 
# Claudia Martínez-Alcalá, Génesis Vázquez-Tagle, 
# Margarita Tetlalmatzi-Montiel, Jorge Viveros, and 
# José Sócrates López-Noguerola (2017), STATIONARITY DURING REM SLEEP IN 
# OLD ADULTS, Alzheimer's & Dementia, Volume #, Issue #, 2017, Pages P#, 
# ISSN 1552-5260.
# https://alz.confex.com/alz/2017/aaic/papers/index.cgi?username=16326&password=181472
#

#################################################
# volver a la carpeta central
setwd("C:/Users/Erika/Desktop/Julio161213/scripts170620")
dir_actual  = getwd()

###############################################################################
# parametros del script, ver documantacion
nombre      = 'CLMN10SUE'
etiqueta    = 'CLMN'
dir_datos   = paste0(getwd(),'/CLMN10SUE')
dir_res     = paste0(getwd(),'/res_parciales')

# nombre      = 'MJNNVIGILOS'
# etiqueta    = 'MJNN'
# dir_datos   = paste0(getwd(),'/MJNNVIGILOScCanal')
# dir_res     = paste0(getwd(),'/res_parciales')

reemplazar  = TRUE
fr_muestreo = 512
dur_epoca   = 30
canales     = 'PSG'
# canales     = c('C3','C4','CZ','F3','F4','F7','F8','FP1','FP2','FZ','O1','O2',
#                 'P3','P4','PZ','ROG','T3','T4','T5','T6')

reemplazar  = TRUE
grabar      = FALSE
anotaciones = ''

zoom           = TRUE
unidad_par_t   = 'tiempo'
ajuste_ini_hms = c(1,0,0)
min_hms        = c(1,30,0)
max_hms        = c(1,31,0)
#unidad_par_t   = 'puntos'
#ajuste_ini_epo = 0
#min_epo        = 0
#max_epo        = 0

#################################################
# parametros de dibujo
escala_mv  = 300
salto      = 8
sep_lineas = 60
n_tiempos  = 10

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
# parametros que dependen de los datos
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
if(unidad_par_t =='epoca'){
  ini_epo = ajuste_ini_epo
  ini_t   = ini_epo*dur_epoca
  ini_pt  = ini_epo*ventana
}
str_t   = 0
str_epo = 0
str_pt  = 1

# ini : cuando inicia el archivo, util si es un fragmento
# str : cuando inicia el zoom, evita la confusion provocada por ini

# procesamiento parametros opcionales (zoom)
if(zoom){
  confirma_zoom = FALSE
  if(unidad_par_t == 'tiempo'){
    min_t  = min_hms[1]*60*60 + min_hms[2]*60 + min_hms[3] -ini_t
    max_t  = max_hms[1]*60*60 + max_hms[2]*60 + max_hms[3] -ini_t
    
    min_e  = (min_t+ini_t)/dur_epoca -ini_epo
    max_e  = (max_t+ini_t)/dur_epoca -ini_epo
    
    confirma_zoom = TRUE
  }
  if(unidad_par_t == 'puntos'){
    min_t = min_epo*dur_epoca -ini_t
    max_t = max_epo*dur_epoca -ini_t
    
    min_e = (min_epo+ini_t) -ini_epo
    max_e = (max_epo+ini_t) -ini_epo
    
    confirma_zoom = TRUE
  }
  if(!confirma_zoom){
    warning('WARNING: Indique unidad de tiempo para zoom (epocas o segundos)')
  }
  min_pt = floor(min_t*fr_muestreo)
  max_pt = ceiling(max_t*fr_muestreo)
  
  str_t   = max(min_t, 0)
  str_epo = max(min_e, 0)
  str_pt  = max(min_pt,1)
}

#################################################
# inicia guardado de los graficos
if(grabar){
  setwd(dir_res)
  png(paste0(etiqueta,'_',
  #pdf(paste0(etiqueta,'_',
             '_espectro',
  #           '.pdf'),width=12,height=6)
             '.png'),units='cm',res=300,width=20,height=11)
}

#################################################
# modo seguro: revisa si estan los archivos
correctos = rep(FALSE,n_canales)
n_datos   = Inf

setwd(dir_datos)
for(ch in 1:n_canales){
  ch_actual = canales[ch]
  if(file.exists(paste0(nombre,'_',ch_actual,'.txt'))){
    correctos[ch] =TRUE
  }else{
    warning('ERROR: En canal ',ch_actual,'no se encontro el archivo ',
            paste0(nombre,'_',ch_actual,'.txt'))
  }
}
canales   = canales[correctos]
n_canales = length(canales)

#################################################
# graficacion

# carga los datos para inicializar la ventana
setwd(dir_datos)
ch        = 1
ch_actual = canales[ch]
nom_arch  = paste0(nombre,'_',ch_actual,'.txt')
DATA      = scan(nom_arch)
n_datos   = length(DATA)

# ajustes en el tiempo
if(zoom){
  end_t   = min(max_t, n_datos/fr_muestreo)
  end_epo = min(max_e, n_datos/ventana)
  end_pt  = min(max_pt,n_datos)
  
  DATA    = DATA[str_pt:end_pt]
  
  ini_t   = ini_t   + str_t
  ini_epo = ini_epo + str_epo
  ini_pt  = ini_pt  + str_pt
  n_datos   = length(DATA)
} 
# end : cuando termina el zoom, evita la confusion provocada por ini

# miscelanea de indices para detalles tecnicos
n_puntos  = floor(n_datos/salto)
ind_t     = floor(seq(0,n_puntos,by=n_puntos/n_tiempos))+1
ind_pt    = seq(0,n_datos,by=salto)
ind_pt[1] = 1
ind_linea = seq(0,n_datos,by=sep_lineas*fr_muestreo)
ind_linea[1] = 1

# creacion etiquetas de tiempo
num_t  = (0:n_puntos)*(salto/fr_muestreo) + ini_t
num_t  = num_t[ind_t]
num_hh = floor(num_t/(60*60))
num_mm = floor( (num_t - num_hh*60*60)/60 )
num_ss = floor(  num_t - num_hh*60*60 - num_mm*60 )
num_ms = num_t - num_hh*60*60 - num_mm*60 - num_ss
txt_t  = character(length(num_t))
for(i in 1:(length(num_t))){
  txt_mm = toString(num_mm[i])
  if(num_mm[i]<10){
    txt_mm = paste0('0',num_mm[i])
  }
  txt_ss = toString(num_ss[i])
  if(num_ss[i]<10){
    txt_ss = paste0('0',num_ss[i])
  }
  txt_t[i] = paste0(toString(num_hh[i]),':',txt_mm,':',txt_ss)
}

#################################################
# colores del grafico
vec_colores = c('black','black','black',
                'blue','blue','blue','blue',
                'blueviolet','blueviolet','blueviolet',
                'black','black','red','red','red',
                'black','black','black','black',
                'red','red',
                'black')

#################################################
# graficacion

#####################################################################
#  FUNCIONES VENTANA

# Funciion de respuesta a frecuencia
# Tukey-Hanning
gg = function(uu,MM){
  val = 0
  if(abs(uu)<MM){
    val = 0.5 + 0.5*cos(pi*uu/MM)#/(pi*uu/MM)
  }
  return(val)
}

# Ventana de retrasos
# Tukey-Hanning
ll = function(uu,MM){
  val = 0
  if(abs(uu)<MM){
    val = 0.5 + 0.5*cos(pi*uu/MM)#/(pi*uu/MM)
  }
  return(val)
}

#####################################################################
#  ESTIMADOR DE DOBLE VENTANA

# librerias
library(plotrix)
library(rgl)

# parametros
n_om  = 100   # numero de frecuencias
debug = T

# datos ejemplo
#Xt = ej_XX
Xt = DATA

# datos dados
TT = length(Xt)

# variables de apoyo
OM = seq(0,pi,pi/n_om)
OO = length(OM)
ii = complex(real=0,imaginary=1)

UU = matrix(nrow=TT,ncol=OO)

MM = 512/8

for(ww in 1:OO){
  wi = OM[ww]
  for(ti in 1:TT){
    U_tmp = 0
    
    subb = max(1 ,ti-MM/2)
    supp = min(TT,ti+MM/2)
    for(tj in subb:supp){
      U_tmp = U_tmp + gg(ti-tj,MM)*Xt[tj]*exp(ii*wi*tj)
    }
    UU[ti,ww] = U_tmp 
  }
  print(toString(floor(ww/OO*100*10)/10))
}

UU = Mod(UU)**2

#color2D.matplot(t(UU),border=NA,
#                main='Estimador U**2')

#color2D.matplot(log(t(UU)),border=NA,
#                main='Estimador log(U**2)')

#jet.colors <-   # function from grDevices package
#  colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
#                     "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
#colorzjet <- jet.colors(100)  # 100 separate color 
#col.ind <- cut(log(UU),100)
#open3d()
#persp3d(x=1:TT,y=1:OO,z=log(UU),col=colorzjet[col.ind])

#heatmap(log(t(UU)),Rowv=NA,Colv=NA)

ff = matrix(nrow=TT,ncol=OO)

for(ww in 1:OO){
  wi = OM[ww]
  for(ti in 1:TT){
    f_tmp = 0
    
    subb = max(1 ,ti-MM/2)
    supp = min(TT,ti+MM/2)
    for(tj in subb:supp){
      f_tmp = f_tmp + ll(ti-tj,MM)*UU[tj,ww]
    }
    ff[ti,ww] = f_tmp 
  }
  print(toString(floor(ww/OO*100*10)/10))
}

color2D.matplot(t(ff),border=NA,
                main='Estimador f')

color2D.matplot(log(t(ff)),border=NA,
                main='Estimador Y = log(f)')


jet.colors <-   # function from grDevices package
  colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                     "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
colorzjet <- jet.colors(100)  # 100 separate color 
col.ind <- cut(log(ff),100)
open3d()
persp3d(x=1:TT,y=1:OO,z=log(ff),col=colorzjet[col.ind],
        main=paste0(etiqueta,' , ',canal))
