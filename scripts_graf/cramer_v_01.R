###############################################################################
# directorio de trabajo
dir_actual = 'C:/Users/EQUIPO 1/Desktop/julio/tesis_respaldo/TESIS/scripts_graf'
dir_graf   = 'C:/Users/EQUIPO 1/Desktop/julio/tesis_respaldo/TESIS/scripts_graf_res'
info_dir   = 'C:/Users/EQUIPO 1/Desktop/julio/tesis_respaldo/TESIS/scripts_graf'
dir_datos  = 'C:/Users/EQUIPO 1/Desktop/julio/estacionariedad_180408'

###############################################################################
# parametros
#sujeto     = 2

orden_stam = T

###############################################################################
# librerias
require('readxl')

require('ggplot2')
require('ggpubr')

require('Rmisc')
require('reshape')

require('scales')

require('hms')

###############################################################################
# datos generales
info     = read_excel(paste0(info_dir,'/info_tecnico.xlsx'))

orden_k  = read_excel(paste0(info_dir,'/info_canales.xlsx'))
kanales  = read_excel(paste0(info_dir,'/info_canales.xlsx'),
                      sheet='Alfabetico')
if(orden_stam){
  kanales  = read_excel(paste0(info_dir,'/info_canales.xlsx'),
                        sheet='Stam')
}
n.canales    = length(kanales$Etiqueta)
canales.arch = kanales$Nombre_archivo

###############################################################################
# parametros del script
setwd(dir_actual)
source('utileria.R')

###############################################################################
# parametros del script
nombre      = info$Nombre_archivo[sujeto]
etiqueta    = info$Nombre[sujeto]
dir_res_mid = dir_datos
fr_muestreo = info$Fr_muestreo[sujeto]

binario = T

zoom           = T
unidad_par_t   = 'tiempo'
ajuste_ini_hms = c(0,0,0)
min_hms        = c(0,0,0)
max_hms        = c(info$hh_ff[sujeto],info$mm_ff[sujeto],0)
#unidad_par_t   = 'puntos'
#ajuste_ini_epo = 0
#min_epo        = epoca_ini[sujeto]
#max_epo        = epoca_fin[sujeto]-1
#min_epo        = 0

#################################################
# parametros que dependen del sujeto
if(info$Grupo_n[sujeto]==0){
  grupo = 'CTRL'
}
if(info$Grupo_n[sujeto]==1){
  grupo = 'PDCL'
}
if(info$Grupo_n[sujeto]==-1){
  grupo = 'EX'
}

#################################################
# epocas de suenno MOR
ar_indice = read_excel(paste0(info_dir,'/info_tecnico.xlsx'),
                       sheet='EpocasTesis')
indice    = ar_indice[,etiqueta]
indice    = as.numeric(unlist(indice))
indice    = indice[!is.na(indice)]

if(fr_muestreo==200){
  indixe = ceiling(indice/3)
  indixe = unique(indixe)
  indixe = sort(indixe)
  indice = indixe
}

###############################################################################
# meta-graficacion
require(questionr)

cv.test = function(x,y) {
  CV = sqrt(chisq.test(x, y, correct=FALSE)$statistic /
              (length(x) * 
                 (min(length(unique(x)),length(unique(y))) - 1)))
  #print.noquote("Cramér V / Phi:")
  return(as.numeric(CV))
}

#for(expon in c(2,0,-2)){
#for(expon in (-5):-2){
#for(expon in (-1):(2)){
#for(expon in (-5):(1)){
for(expon in c(0)){
  dur_epoca = 30*(2**expon)
  setwd(dir_actual)
  source('cramer_v_01_parte.R')
}
