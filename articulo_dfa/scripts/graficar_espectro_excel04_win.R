###############################################################################
# parametros
grabar         = FALSE
potencia.total = T
#sujeto         = 2
orden_stam     = TRUE

usar.log = T

# tamano de la ventana analizada, nombre raro para evitar confusiones
dur.chunk   = 1

quitar.artefactos = TRUE

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
  tt = read_csv(paste0(dir_graf,'/espectro',info$Nombre_archivo[sujeto],
                       '_',agregado,'.csv'))
  RES.nuevo = rbind(RES.nuevo,tt)
}

RES.nuevo = cbind(RES.nuevo,
                  (RES.nuevo$Delta+RES.nuevo$Theta)/(RES.nuevo$Alfa+RES.nuevo$Beta))
colnames(RES.nuevo)[14] = 'Ratio'

if(usar.log){
  RES.nuevo$Delta    = log(RES.nuevo$Delta)
  RES.nuevo$Theta    = log(RES.nuevo$Theta)
  RES.nuevo$Alfa     = log(RES.nuevo$Alfa)
  RES.nuevo$Beta     = log(RES.nuevo$Beta)
  RES.nuevo$Gamma    = log(RES.nuevo$Gamma)
  RES.nuevo$Varianza = log(RES.nuevo$Varianza)
  RES.nuevo$Ratio    = log(RES.nuevo$Ratio)
}

parche = RES.nuevo[grep(1:9,RES.nuevo$Canal),]
parche$Canal        = factor(parche$Canal,
                                labels=canales$Etiqueta)
parche$Participante = factor(RES.nuevo$Participante,
                                labels = info$Etiqueta[sujetos])
parche$Grupo        = factor(RES.nuevo$Grupo,
                                labels=c('CTL','PDC'))
parche$Etapa        = factor(RES.nuevo$Etapa,labels=c('NMOR','MOR'))

ggplot(RES.nuevo,aes(x=Canal,y=Ratio,fill=Grupo)) +
  geom_violin() +
  xlab(NULL) + ylab('Área bajo la curva') +
  labs(title=paste0('Razón de enlentecimiento (',agregado,')')) +
  theme(legend.position='bottom') +
  stat_compare_means(label = 'p.signif',method='t.test')+
  labs(subtitle='Transformación logarítmica del espectro integrado') +
  facet_grid(Etapa~.)+
  theme_pubclean()+
  rotate_x_text(angle = 45)

RES.nuevo$Participante = factor(RES.nuevo$Participante,
                                labels = info$Etiqueta[sujetos])
RES.nuevo$Canal        = factor(RES.nuevo$Canal,
                                labels=canales$Etiqueta)
RES.nuevo$Grupo        = factor(RES.nuevo$Grupo,
                                labels=c('CTL','PDC'))
RES.nuevo$Etapa        = factor(RES.nuevo$Etapa,labels=c('NMOR','MOR'))

###############################################################################
# NMOR vs MOR
ggplot(RES.nuevo,aes(x=Canal,y=Delta,fill=Grupo)) +
  geom_boxplot() +
  xlab(NULL) + ylab('Área bajo la curva') +
  labs(title=paste0('Ondas Delta (',agregado,')')) +
  theme(legend.position='bottom') +
  stat_compare_means(label = 'p.signif',method='t.test')+
  labs(subtitle='Transformación logarítmica del espectro integrado') +
  facet_grid(Etapa~.)+
  rotate_x_text(angle = 45)

ggplot(RES.nuevo,aes(x=Canal,y=Theta,fill=Grupo)) +
  geom_boxplot() +
  xlab(NULL) + ylab('Área bajo la curva') +
  labs(title=paste0('Ondas Theta (',agregado,')')) +
  theme(legend.position='bottom') +
  stat_compare_means(label = 'p.signif',method='t.test')+
  labs(subtitle='Transformación logarítmica del espectro integrado') +
  facet_grid(Etapa~.)+
  rotate_x_text(angle = 45)

ggplot(RES.nuevo,aes(x=Canal,y=Alfa,fill=Grupo)) +
  geom_boxplot() +
  xlab(NULL) + ylab('Área bajo la curva') +
  labs(title=paste0('Ondas Alfa (',agregado,')')) +
  theme(legend.position='bottom') +
  stat_compare_means(label = 'p.signif',method='t.test')+
  labs(subtitle='Transformación logarítmica del espectro integrado') +
  facet_grid(Etapa~.)+
  rotate_x_text(angle = 45)  

ggplot(RES.nuevo,aes(x=Canal,y=Beta,fill=Grupo)) +
  geom_boxplot() +
  xlab(NULL) + ylab('Área bajo la curva') +
  labs(title=paste0('Ondas Beta (',agregado,')')) +
  theme(legend.position='bottom') +
  stat_compare_means(label = 'p.signif',method='t.test')+
  labs(subtitle='Transformación logarítmica del espectro integrado') +
  facet_grid(Etapa~.)+
  rotate_x_text(angle = 45)

ggplot(RES.nuevo,aes(x=Canal,y=Gamma,fill=Grupo)) +
  geom_boxplot() +
  xlab(NULL) + ylab('Área bajo la curva') +
  labs(title=paste0('Ondas Gamma (',agregado,')')) +
  theme(legend.position='bottom') +
  stat_compare_means(label = 'p.signif',method='t.test')+
  labs(subtitle='Transformación logarítmica del espectro integrado') +
  facet_grid(Etapa~.)+
  rotate_x_text(angle = 45)

ggplot(RES.nuevo,aes(x=Canal,y=Varianza,fill=Grupo)) +
  geom_boxplot() +
  xlab(NULL) + ylab('Área bajo la curva') +
  labs(title=paste0('Potencia total (',agregado,')')) +
  theme(legend.position='bottom') +
  stat_compare_means(label = 'p.signif',method='t.test')+
  labs(subtitle='Transformación logarítmica del espectro integrado') +
  facet_grid(Etapa~.)+
  rotate_x_text(angle = 45)

ggplot(RES.nuevo,aes(x=Canal,y=Ratio,fill=Grupo)) +
  geom_boxplot() +
  xlab(NULL) + ylab('Área bajo la curva') +
  labs(title=paste0('Razón de enlentecimiento (',agregado,')')) +
  theme(legend.position='bottom') +
  stat_compare_means(label = 'p.signif',method='t.test')+
  labs(subtitle='Transformación logarítmica del espectro integrado') +
  facet_grid(Etapa~.)+
  theme_pubclean()+
  rotate_x_text(angle = 45)


parche = subset.data.frame(RES.nuevo,Canal)

ggplot(RES.nuevo,aes(x=Canal,y=Ratio,fill=Grupo)) +
  geom_violin() +
  xlab(NULL) + ylab('Área bajo la curva') +
  labs(title=paste0('Razón de enlentecimiento (',agregado,')')) +
  theme(legend.position='bottom') +
  stat_compare_means(label = 'p.signif',method='t.test')+
  labs(subtitle='Transformación logarítmica del espectro integrado') +
  facet_grid(Etapa~.)+
  theme_pubclean()+
  rotate_x_text(angle = 45)

##################################################3

ggplot(RES.nuevo,aes(x=Canal,y=Varianza,fill=Grupo)) +
  geom_boxplot() +
  xlab(NULL) + ylab('Área bajo la curva') +
  labs(title=paste0('Potencia total (',agregado,')')) +
  theme(legend.position='bottom') +
  stat_compare_means(label = 'p.signif',method='t.test')+
  labs(subtitle='Transformación logarítmica del espectro integrado') +
  facet_grid(Etapa~.)+
  theme_pubclean()+
  rotate_x_text(angle = 45)

ggplot(RES.nuevo,aes(x=Canal,y=Varianza,fill=Etapa)) +
  geom_boxplot() +
  xlab(NULL) + ylab('Área bajo la curva') +
  labs(title=paste0('Potencia total (',agregado,')')) +
  theme(legend.position='bottom') +
  stat_compare_means(label = 'p.signif',method='t.test')+
  labs(subtitle='Transformación logarítmica del espectro integrado') +
  facet_grid(Grupo~.)+
  theme_pubclean()+
  rotate_x_text(angle = 45)

###############################################################################
# MOR vs NMOR
ggplot(RES.nuevo,aes(x=Canal,y=(Delta),fill=Etapa)) +
  geom_boxplot() +
  xlab(NULL) + ylab('Área bajo la curva') +
  labs(title=paste0('Ondas Delta (',agregado,')')) +
  theme(legend.position='bottom') +
  stat_compare_means(label = 'p.signif',method='t.test')+
  labs(subtitle='Transformación logarítmica del espectro integrado') +
  facet_grid(Grupo~.)+
  rotate_x_text(angle = 45)
ggsave(filename='ondas_delta.eps',device='eps')

ggplot(RES.nuevo,aes(x=Canal,y=exp(Theta),fill=Etapa)) +
  geom_boxplot() +
  xlab(NULL) + ylab('Absolute Power') +
  labs(title='Theta band') +
  theme(legend.position='bottom') +
  stat_compare_means(label = 'p.signif',method='t.test')+
  #labs(subtitle='Transformación logarítmica del espectro integrado') +
  facet_grid(Grupo~.)+
  rotate_x_text(angle = 45)
ggsave(filename='ondas_theta_2_relatiev.eps',device='eps')

ggplot(RES.nuevo,aes(x=Canal,y=Alfa,fill=Etapa)) +
  geom_boxplot() +
  xlab(NULL) + ylab('Área bajo la curva') +
  labs(title=paste0('Ondas Alfa (',agregado,')')) +
  theme(legend.position='bottom') +
  stat_compare_means(label = 'p.signif',method='t.test')+
  labs(subtitle='Transformación logarítmica del espectro integrado') +
  facet_grid(Grupo~.)+
  rotate_x_text(angle = 45)  

ggplot(RES.nuevo,aes(x=Canal,y=Beta,fill=Etapa)) +
  geom_boxplot() +
  xlab(NULL) + ylab('Área bajo la curva') +
  labs(title=paste0('Ondas Beta (',agregado,')')) +
  theme(legend.position='bottom') +
  stat_compare_means(label = 'p.signif',method='t.test')+
  labs(subtitle='Transformación logarítmica del espectro integrado') +
  facet_grid(Grupo~.)+
  rotate_x_text(angle = 45)

ggplot(RES.nuevo,aes(x=Canal,y=Gamma,fill=Etapa)) +
  geom_boxplot() +
  xlab(NULL) + ylab('Área bajo la curva') +
  labs(title=paste0('Ondas Gamma (',agregado,')')) +
  theme(legend.position='bottom') +
  stat_compare_means(label = 'p.signif',method='t.test')+
  labs(subtitle='Transformación logarítmica del espectro integrado') +
  facet_grid(Grupo~.)+
  rotate_x_text(angle = 45)

###############################################################################
###############################################################################
