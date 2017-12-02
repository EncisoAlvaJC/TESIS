###############################################################################
# directorio de trabajo
dir_actual = '~/TESIS/TESIS/img_ejemplos'
dir_graf   = '~/TESIS/TESIS/img_art_dfa'
info_dir    = '~/TESIS/TESIS/articulo_dfa'
dir_datos  = '~/TESIS/graf_datos/estacionariedad_sf/'
dir_epocas = '~/TESIS/graf_datos/epocas3/'

###############################################################################
# parametros
#sujeto     = 2
grabar_tot  = F
grabar      = F
anotaciones = ''

orden_stam = T

# parametros de dibujo
paso    = 15*2

###############################################################################
# librerias
require('readxl')

require('ggplot2')
require('ggpubr')

require('Rmisc')
require('reshape')

require('scales')

require('squash')

###############################################################################
# datos generales
info     = read_excel(paste0(info_dir,'/info_tecnico.xlsx'))

orden_k  = read_excel(paste0(info_dir,'/info_canales.xlsx'))
kanales  = read_excel(paste0(info_dir,'/info_canales.xlsx'))
if(orden_stam){
  kanales  = read_excel(paste0(info_dir,'/info_canales_alterno.xlsx'))
}
n.canales    = length(kanales$Etiqueta)
canales.arch = kanales$Nombre_archivo

###############################################################################
# parametros del script
nombre      = info$Nombre_archivo[sujeto]
etiqueta    = info$Nombre[sujeto]
dir_res_mid = dir_datos
fr_muestreo = info$Fr_muestreo[sujeto]

binario = T

zoom           = F
unidad_par_t   = 'tiempo'
ajuste_ini_hms = c(0,0,0)
#min_hms        = c(3,17,20)
#max_hms        = c(3,19,10)
#unidad_par_t   = 'puntos'
#ajuste_ini_epo = 0
#min_epo        = epoca_ini[sujeto]
#max_epo        = epoca_fin[sujeto]-1
#min_epo        = 0

#################################################
# parametros que dependen del sujeto
if(info$Grupo_n[sujeto]==0){
  grupo = 'CTL'
}
if(info$Grupo_n[sujeto]==1){
  grupo = 'PDC'
}
if(info$Grupo_n[sujeto]==-1){
  grupo = 'EX'
}

#################################################
# inicia grafico
if(grabar_tot){
  setwd(dir_actual)
  #pdf(
  png(
    paste0(nombre,'_comp_est_',
           #'.pdf'),width=5.941*k,height=1*k)
           '.png'),units='in',res=300,width=5.941*k,height=9*k)
}

###############################################################################
# meta-graficacion

stop('Hasta aqui bien')

setwd(dir_actual)
dur_epoca = 30/(2**5)
par(fig=c(0,1,cont,cont+qq), new=FALSE)
source('~/TESIS/TESIS/img_ejemplos/colorcitos_usable03_parte.R')
cont = cont + qq

for(expon in -4:2){
  setwd(dir_actual)
  dur_epoca = 30*(2**expon)
  par(fig=c(0,1,cont,cont+qq), new=TRUE)
  source('~/TESIS/TESIS/img_ejemplos/colorcitos_usable03_parte.R')
  cont = cont + qq
}

# el titulo
par(oma=c(0,0,0,0),
    mar=c(0, 2, 2.5, 2),
    mgp=c(0,.5,0),
    fig=c(0,1,.95,1), new=TRUE)
title(paste0('Sujeto : ',etiqueta,'  | Grupo :  ',grupo),cex.main=2)

setwd(dir_actual)
if(grabar_tot){
  setwd(dir_actual)
  dev.off()
}
# fin guardado automatico del grafico
#################################################