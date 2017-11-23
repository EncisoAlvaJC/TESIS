###############################################################################
# parametros
grabar      = FALSE
no_relativo = TRUE
#sujeto = 2

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
dir_graf    = 'C:/Users/EQUIPO 1/Desktop/julio/TESIS/articulo_dfa'

###############################################################################
# datos generales
info = read_excel(paste0(dir_info,'/info.xlsx'))

v.nombres     = info$Nombre_archivo
v.etiqueta    = info$Etiqueta
v.directorio  = info$Nombre_directorio
frecuenciasss = info$Fr_muestreo
grupo_de      = info$Grupo_n

h_ini = info$ini_hh
m_ini = info$ini_mm
s_ini = info$ini_ss

h_fin = info$fin_hh
m_fin = info$fin_mm
s_fin = info$fin_ss

banda.n = c('Delta','Theta','Alfa','Beta','Gamma','Potencia total',
            'Ondas lentas fuera de rango','Ondas rapidas fuera de rango')
banda   = c('DELTA','THETA','ALFA','BETA','GAMMA','TOTAL','SUB','SUPER')
nbandas = length(banda.n)

###############################################################################
# parametros fijados
stam         = T
orden_stam  = c(9,8,7,6,5,4,17,16,2,1,19,18,14,13,12,11,10,3,15,20,21,22)

dur_epoca   = 1
if(fr_muestreo==512){
  dur_epo_reg = 30
}
if(fr_muestreo==200){
  dur_epo_reg = 10
}

###############################################################################
# parametros del script
nombre      = v.nombres[sujeto]
etiqueta    = v.etiqueta[sujeto]
fr_muestreo = frecuenciasss[sujeto]

# reemplazar  = T
# canales      = 'PSG'
canales = c('C3','C4','CZ','F3','F4','F7','F8','FP1','FP2','FZ','O1','O2',
            'P3','P4','PZ','T3','T4','T5','T6')
if(stam){
  orden_stam  = c(9,8,7,6,5,4,17,16,2,1,19,18,14,13,12,11,10,3,15)
  canales = canales[orden_stam]
}

zoom           = F
unidad_par_t   = 'tiempo'
ajuste_ini_hms = c(h_ini[sujeto],m_ini[sujeto],s_ini[sujeto])
min_hms        = c(h_ini[sujeto],m_ini[sujeto],s_ini[sujeto])
max_hms        = c(h_fin[sujeto],m_fin[sujeto],s_fin[sujeto])
#unidad_par_t   = 'epocas'
#ajuste_ini_epo = 0
#min_epo        = 0
#max_epo        = 0

#################################################
# VESTIGIAL
# if(reemplazar){
#   if(canales=='10-20'){
#     canales = c('C3','C4','CZ','F3','F4','F7','F8','FP1','FP2','FZ','O1','O2',
#                 'P3','P4','PZ','T3','T4','T5','T6')
#     if(stam){
#       orden_stam  = c(9,8,7,6,5,4,17,16,2,1,19,18,14,13,12,11,10,3,15)
#       canales = canales[orden_stam]
#     }
#   }
#   if(canales=='PSG'){
#     canales = c('C3','C4','CZ','F3','F4','F7','F8','FP1','FP2','FZ','O1','O2',
#                 'P3','P4','PZ','T3','T4','T5','T6','LOG','ROG','EMG')
#     if(stam){
#       orden_stam  = c(9,8,7,6,5,4,17,16,2,1,19,18,14,13,12,11,10,3,15,20,21,22)
#       canales = canales[orden_stam]
#     }
#   }
# }
# if(length(canales)<1){
#   stop('ERROR: Lista de canales tiene longitud cero')
# }

#################################################
# parametros que dependen del sujeto
if(grupo_de[sujeto]==0){
  grupo = 'CTL'
}
if(grupo_de[sujeto]==1){
  grupo = 'PDC'
}
if(grupo_de[sujeto]==-1){
  grupo = 'EX'
}

#################################################
# ajustes del zoom
if(zoom){
  validar.zoom = F
  if(unidad_par_t=='tiempo'){
    s.ini = sum(min_hms*c(60*60,60,1))
    s.fin = sum(max_hms*c(60*60,60,1))
    
    e.ini = s.ini/dur.min + 1
    e.fin = s.fin/dur.min
    
    n.epo = e.fin-e.ini
    
    validar.zoom = T
  }
  if(unidad_par_t=='epocas'){
    s.ini = 
    s.fin = sum(max_hms*c(60*60,60,1))
    
    e.ini = s.ini/dur.min + 1
    e.fin = s.fin/dur.min
    
    n.epo = e.fin-e.ini
    
    validar.zoom = T
  }
}

###############################################################################

#################################################
# inicia grafico
k = 1.5
setwd(dir_actual)
if(grabar_tot){
  if(no_relativo){
    tag = 'total'
  }else{
    tag = 'relativo'
  }
  
  setwd(dir_actual)
  #pdf(
  png(
    paste0(nombre,'_espectral_',tag,
           #'.pdf'),width=5.941*k,height=1*k)
           '.png'),units='in',res=300,width=5.941*k,height=9*k)
}

###############################################################################
# meta-graficacion

# contadores
qq   = .925/7
cont = .025+6*qq

# grafico principal
par(oma=c(0,0,0,0),
    mar=c(.25, 2+1.5, .25, 3+1),
    mgp=c(1.5,.5,0))

setwd(dir_actual)
par(fig=c(0,1,cont,cont+qq), new=FALSE) #new=FALSE)
que.banda = 1
source('C:/Users/EQUIPO 1/Desktop/julio/TESIS/reportar_resultados/graf_espectro_integrado04_parte.R')

# graficacion de epocas
# par(fig=c(0,1,.02,.955), new=TRUE,
#     mar=c(0,2+1.5,0,3+1),
#     mgp=c(0,-.4,0),
#     oma=c(0,0,0,0))
# setwd(dir_epocas)
# arch_indice_e = paste0('epocas_mor_',nombre,'.txt')
# indice_e      = scan(arch_indice_e)
# factor.extra = 1
# if(fr_muestreo==200){
#   factor.extra = 3
# }
# indice_e = indice_e*30/factor.extra - s.ini
# indice_e = indice_e*dur_epoca
# #plot(0,type='n',xaxt='n',yaxt='n',xlab='',ylab='',#bty='n',
# #     xlim=c(0,n_epocas)+.5,ylim=c(0,1))
# te = t(RES[rev(1:n_canales),])*0-1
# te[1,1]=.2
# te[1,2]=.2
# colorgram(z=te,outlier='white',
#           colFn = grayscale,
#           bty='n',axes=F,
#           xlab='Tiempo [hh]',ylab='',zlab='',
#           breaks=seq(0,1),
#           key=0)
# for(i in indice_e){
#   #rect(i/(factor.extra/2),-1,
#   #     (i+1)/(factor.extra/2),25,
#   #     col='green',
#   #     border=NA)
#   rect(i-30/(factor.extra),0.5,
#        i,22.5,
#        col=rgb(128,255,128,
#                #alpha=128,
#                maxColorValue=255),
#        border=NA) 
# }
# skip = seq(1,n_epocas+1,by=paso*4)+.5
# skap = seq(1,length(txt_t),by=4*15*60/dur_epoca)
# #axis(1,at=skip-1,labels=txt_t[skap],las=1,tick=F)
# 
# grafico principal
par(oma=c(0,0,0,0),
   mar=c(.25, 2+1.5, .25, 3+1),
   mgp=c(1.5,.5,0))

cont = cont + qq
que.banda = 0
for(qb in 1:5){
 cont = cont - qq
 setwd(dir_actual)
 que.banda = que.banda + 1 
 par(fig=c(0,1,cont,cont+qq), new=TRUE)
 source('C:/Users/EQUIPO 1/Desktop/julio/TESIS/reportar_resultados/graf_espectro_integrado04_parte.R')
}

# potencia total
cont = cont - qq
setwd(dir_actual)
par(fig=c(0,1,cont,cont+qq), new=TRUE)
source('C:/Users/EQUIPO 1/Desktop/julio/TESIS/img_resultados/graf_espectro_integrado04_varianza.R')

# pseudo-color
cont = cont - qq
setwd(dir_actual)
par(fig=c(0,1,cont,cont+qq), new=TRUE)
source('C:/Users/EQUIPO 1/Desktop/julio/TESIS/img_resultados/graf_espectro_integrado04_exponente.R')

#qq = qq*(2/3)

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