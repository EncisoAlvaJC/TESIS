###############################################################################
# parametros
grabar         = FALSE
potencia.total = F
#sujeto         = 2
orden_stam     = TRUE

usar.log = T

# tamano de la ventana analizada, nombre raro para evitar confusiones
dur.chunk   = 30

zoom           = T
unidad_par_t   = 'tiempo'
#unidad_par_t   = 'epocas'

###############################################################################
# directorios de trabajo
#
#     gral : de uso general
#     info : detalles de los participantes
#  scripts : sub-rutinas, en caso de haberlas
#  res_pre : resultados previos, solo para analizar y/o graficar
#   epocas : epocas para resaltar, por ahora solo MOR
#     graf : donde guardar los graficos, en caso de producirse

dir_gral    = 'C:/Users/EQUIPO 1/Desktop/julio/TESIS/articulo_dfa'
dir_info    = 'C:/Users/EQUIPO 1/Desktop/julio/TESIS/articulo_dfa'
dir_scripts = 'C:/Users/EQUIPO 1/Desktop/julio/TESIS/articulo_dfa'
dir_res_pre = 'C:/Users/EQUIPO 1/Desktop/julio/espectro_integrado_171018'
dir_epocas  = 'C:/Users/EQUIPO 1/Desktop/julio/epocas_dfa'
dir_graf    = 'C:/Users/EQUIPO 1/Desktop/julio/TESIS/articulo_dfa/espectro_tmp'

###############################################################################
# librerias
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jdk1.8.0_65')
require(readxl)
require(xlsx)

require(ggplot2)
require(ggpubr)

require(readr)

# sub-rutinas que acortan el codigo
source(paste0(dir_scripts,'/utileria.R'))

###############################################################################
# datos generales
info     = read_excel(paste0(dir_info,'/info_participantes.xlsx'))
bandas   = read_excel(paste0(dir_info,'/info_bandas.xlsx'))

n.bandas = length(bandas$Banda)

canales  = read_excel(paste0(dir_info,'/info_canales.xlsx'))
if(orden_stam){
  canales  = read_excel(paste0(dir_info,'/info_canales_alterno.xlsx'))
}
n.canales = length(canales$Etiqueta)

sujetos = 1:9

if(potencia.total){
  agregado = 'total'
}else{
  agregado = 'relativo'
}

RES.nuevo = c()
for(sujeto in sujetos){
  #tt = read_csv(paste0(dir_graf,'/espectro',info$Nombre_archivo[sujeto],
  #                     '_',agregado,'_promedios.csv'))
  tt = read_csv(paste0(dir_graf,'/estacionariedad_',info$Nombre_archivo[sujeto],
                       '_',toString(dur.chunk),'_promedios.csv'))
  RES.nuevo = rbind(RES.nuevo,tt)
}

# if(usar.log){
#   RES.nuevo$Delta = log(RES.nuevo$Delta)
#   RES.nuevo$Theta = log(RES.nuevo$Theta)
#   RES.nuevo$Alfa  = log(RES.nuevo$Alfa)
#   RES.nuevo$Beta  = log(RES.nuevo$Beta)
#   RES.nuevo$Gamma = log(RES.nuevo$Gamma)
#   #RES.nuevo$Ratio = log(RES.nuevo$Ratio)
# }

if(usar.log){
  RES.nuevo$Proporcion = log(RES.nuevo$Proporcion)
}

RES.nuevo$Participante = factor(RES.nuevo$Participante,
                                labels = info$Etiqueta[sujetos])
RES.nuevo$Canal        = factor(RES.nuevo$Canal/2,
                                labels=c(canales$Etiqueta))
RES.nuevo$Grupo        = factor(RES.nuevo$Grupo,
                                labels=c('CTL','PDC'))
RES.nuevo$Etapa        = factor(RES.nuevo$Etapa,labels=c('NMOR','MOR'))

###############################################################################
# NMOR vs MOR
ggplot(RES.nuevo,aes(x=Canal,y=Proporcion,fill=Grupo)) +
  geom_boxplot() +
  xlab(NULL) + ylab('Épocas estacionarias [%]') +
  labs(title=paste0('Estacionariedad (',toString(dur.chunk),'s)')) +
  theme(legend.position='bottom') +
  stat_compare_means(label = 'p.signif',method='t.test')+
  #labs(subtitle='Transformación logarítmica del espectro integrado') +
  facet_grid(Etapa~.)+
  rotate_x_text(angle = 45)

###############################################################################
# MOR vs NMOR
ggplot(RES.nuevo,aes(x=Canal,y=Proporcion,fill=Etapa)) +
  geom_boxplot() +
  xlab(NULL) + ylab('Épocas estacionarias [%]') +
  labs(title=paste0('Estacionariedad (',toString(dur.chunk),'s)')) +
  theme(legend.position='bottom') +
  stat_compare_means(label = 'p.signif',method='t.test')+
  #labs(subtitle='Transformación logarítmica del espectro integrado') +
  facet_grid(Grupo~.)+
  rotate_x_text(angle = 45)

###############################################################################
###############################################################################
