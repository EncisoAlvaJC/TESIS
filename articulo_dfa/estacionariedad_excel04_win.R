# usar la prueba wilcox de rangos "con peso"

###############################################################################
# parametros
grabar         = FALSE
#sujeto         = 2
orden_stam     = TRUE

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
#dir_res_pre = paste0('C:/Users/EQUIPO 1/Desktop/julio/estacionariedad_sf/',)
dir_epocas  = 'C:/Users/EQUIPO 1/Desktop/julio/epocas_dfa'
dir_graf    = 'C:/Users/EQUIPO 1/Desktop/julio/TESIS/articulo_dfa/espectro_tmp'

###############################################################################
# librerias
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jdk1.8.0_65')
require(readxl)
require(xlsx)

require(ggplot2)

# sub-rutinas que acortan el codigo
source(paste0(dir_scripts,'/utileria.R'))

###############################################################################
# datos generales
info     = read_excel(paste0(dir_info,'/info_participantes2.xlsx'))
bandas   = read_excel(paste0(dir_info,'/info_bandas.xlsx'))

n.bandas = length(bandas$Banda)

canales  = read_excel(paste0(dir_info,'/info_canales.xlsx'))
if(orden_stam){
  canales  = read_excel(paste0(dir_info,'/info_canales_alterno.xlsx'))
}
n.canales = length(canales$Etiqueta)

###############################################################################
# parametros del script
nombre      = info$Nombre_archivo[sujeto]
etiqueta    = info$Etiqueta[sujeto]
fr_muestreo = info$Fr_muestreo[sujeto]

dir_res_pre = paste0('C:/Users/EQUIPO 1/Desktop/julio/estacionariedad_sf/',
                     info$Nombre_directorio[sujeto])

#ajuste_ini_hms = c(info$ini_hh[sujeto],info$ini_mm[sujeto],info$ini_ss[sujeto])
#min_hms        = c(info$ini_hh[sujeto],info$ini_mm[sujeto],info$ini_ss[sujeto])
#max_hms        = c(info$fin_hh[sujeto],info$fin_mm[sujeto],info$fin_ss[sujeto])
ajuste_ini_hms = c(0,0,0)
min_hms        = c(info$ini_hh[sujeto],info$ini_mm[sujeto],info$ini_ss[sujeto])
max_hms        = c(info$fin_hh[sujeto],info$fin_mm[sujeto],info$fin_ss[sujeto])
#ajuste_ini_epo = 0
#min_epo        = 0
#max_epo        = 0

grupo = info$Grupo_n[sujeto]
if(grupo== 0){grupo = 'CTL'}
if(grupo== 1){grupo = 'PDC'}
if(grupo==-1){grupo = 'EX'}

#################################################
# ajustes del zoom
if(zoom){
  validar.zoom = F
  if(unidad_par_t=='tiempo'){
    s.ini = hms2t(ajuste_ini_hms)
    s.fin = hms2t(max_hms)
    
    e.ini = s.ini/30+1
    e.fin = s.fin/30+1
    
    n.epo = e.fin - e.ini
    
    validar.zoom = T
  }
  if(unidad_par_t=='epocas'){
    # agregar posteriormente
  }
}

###############################################################################
# leer epocas
archivo.epocas = paste0(dir_epocas,'/epocas_',nombre,'.txt')
indice.epocas  = scan(archivo.epocas)
epo0 = indice.epocas[1]-10
indice.epocas = c(epo0+0:9,indice.epocas)

factor.extra = 1
if(fr_muestreo==200){
  factor.extra = 3
}
indice.chunk   = indice.epocas*30/factor.extra - s.ini #epoca->segundos
indice.chunk   = indice.chunk/dur.chunk               #segundos->chunks
indice.chunk   = ceiling(indice.chunk)
indice.chunk.t = c()
for(k in 1:(30/(factor.extra*dur.chunk))){
  indice.chunk.t = c(indice.chunk.t,indice.chunk+k)
}
indice.chunk = sort((indice.chunk.t))

#################################################
# informacion preeliminar en el archivo
tag = rbind(c('Participante',etiqueta),
            c('Etiqueta',nombre),
            c('Estado',info$Estado[sujeto]),
            c('Fr. muestreo',fr_muestreo),
            c('Dur. epoca',30/factor.extra))

gral           = matrix(ncol=5,nrow=length(indice.epocas))
colnames(gral) = c('Etapa','[#]','Epoca [#]','Inicio [hhmmss]','Fin [hhmmss]')

for(i in 1:10){
  hhmmss1  = t2hms((indice.epocas[i]-1)*30/factor.extra)
  hhmmss2  = corregir.hms(corregir.hms(hhmmss1+c(0,0,30/factor.extra)))
  gral[i,] = c('NMOR',i,indice.epocas[i],hms2txt(hhmmss1),hms2txt(hhmmss2))
}

for(i in 11:length(indice.epocas)){
  hhmmss1  = t2hms((indice.epocas[i]-1)*30/factor.extra)
  hhmmss2  = corregir.hms(hhmmss1+c(0,0,30/factor.extra))
  gral[i,] = c('MOR',i-10,indice.epocas[i],hms2txt(hhmmss1),hms2txt(hhmmss2))
}

#################################################
# optimizacion: lee el tamano de los datos el contenedor
setwd(dir_res_pre)
ch        = 19
ch.actual = canales$Nombre_archivo[ch]
n.archivo = paste0('EST_',nombre,'_',ch.actual,'_T_',toString(dur.chunk),'.txt')
datos.t   = scan(n.archivo)
n.chunks  = length(datos.t)

indice.chunk = indice.chunk[indice.chunk<=n.chunks]

i.d = length(indice.chunk)

RES.todo = as.data.frame(matrix(0,ncol=9,nrow=n.canales*i.d))
colnames(RES.todo) = c('Participante','Canal','EpocaMOR','NumEpoca',
                       'Valido','Estacionario',
                       'Grupo','Etapa','Indice')

RES.suma = as.data.frame(matrix(0,ncol=9,nrow=n.canales*2))
colnames(RES.suma) = c('Participante','Canal','EpocaMOR','NumEpoca',
                       'Epocas','Epocas_estacionarias',
                       'Grupo','Etapa','Proporcion')

RES.todo[,1] = rep(sujeto,n.canales*i.d)
RES.suma[,1] = rep(sujeto,n.canales*2)
if(sujeto<6){
  RES.todo[,7] = rep(0,n.canales*i.d)
  RES.suma[,7] = rep(0,n.canales*2)
}else{
  RES.todo[,7] = rep(1,n.canales*i.d)
  RES.suma[,7] = rep(1,n.canales*2)
}

indize = 1:i.d

#################################################
# contenedores de los datos
RES.t   = matrix(0,nrow=n.canales,ncol=length(indice.chunk))
RES.res = matrix(0,nrow=n.canales,ncol=4)

row.names(RES.t)   = canales$Etiqueta

row.names(RES.res) = canales$Etiqueta
colnames(RES.res)  = c('Total_NMOR','Estacionarios_NMOR',
                       'Total_MOR','Estacionarios_MOR')

#################################################
# inicio ciclo que recorre todos los canales
for(ch in 1:n.canales){
  setwd(dir_res_pre)
  
  # cargar los datos
  ch.actual = canales$Nombre_archivo[ch]
  n.archivo = paste0('EST_',nombre,'_',ch.actual,'_T_',
                     toString(dur.chunk),'.txt')
  datos.t   = scan(n.archivo)
  n.archivo = paste0('EST_',nombre,'_',ch.actual,'_TIR_',
                     toString(dur.chunk),'.txt')
  #datos.tir = scan(n.archivo)
  datos.tir = datos.t
  
  if(zoom){
    datos.t   = datos.t[indice.chunk]
    datos.tir = datos.tir[indice.chunk]
  }
  
  validos  = as.logical(pmin(1*(!is.na(datos.t)),1*(!is.na(datos.tir))))
  validos1 = c(validos[1:(i.d/2)],rep(F,i.d/2))
  validos2 = c(rep(F,i.d/2),validos[(i.d/2+1):i.d])
  
  # organizacion de los datos en una matriz
  RES.res[ch,1]   = sum(1*validos1)
  RES.res[ch,3]   = sum(1*validos2)
  
  datos.t[is.na(datos.t)]     = 1
  datos.tir[is.na(datos.tir)] = 1
  
  RES.t[ch,] = pmin(1*(datos.t<.05),1*(datos.tir<.05))
  
  RES.res[ch,2] = sum(RES.t[ch,validos1])
  RES.res[ch,4] = sum(RES.t[ch,validos2])

  
  step.ch  = i.d*(ch-1)
  
  RES.todo[step.ch+(1:i.d),5] = 1*validos
  RES.todo[step.ch+(1:i.d),6] = RES.t[ch,]
  RES.todo[step.ch+1:i.d,2] = rep(ch,i.d)
  RES.todo[step.ch+1:i.d,9] = indize
  
  RES.suma[ch,5] = RES.res[ch,1]
  RES.suma[ch,6] = RES.res[ch,2]
  RES.suma[ch+n.canales,5] = RES.res[ch,3]
  RES.suma[ch+n.canales,6] = RES.res[ch,4]
  RES.suma[2*ch -1:0,2] = rep(ch,2)
  RES.suma[2*ch-1,8] = 0
  RES.suma[2*ch  ,8] = 1
  
  RES.todo[step.ch+(1:(10*30/(dur.chunk))),8]   = 
    rep(0,10*30/(dur.chunk))
  RES.todo[step.ch+((10*30/(dur.chunk)+1):i.d),8] = 
    rep(1,10*30/(dur.chunk))
}
# fin ciclo que recorre canales
###############################################################################

setwd(dir_graf)

archivo.excel = paste0(nombre,'_estacionariedad_',toString(dur.chunk),'.xlsx')

setwd(dir_graf)

write.xlsx(tag,file=archivo.excel,
           row.names=FALSE,col.names=FALSE,
           sheetName='General',append=FALSE,showNA=TRUE)
write.xlsx(gral,file=archivo.excel,
           row.names=FALSE,col.names=TRUE,
           sheetName='Epocas',append=TRUE,showNA=TRUE)

RES.todo$Canal = factor(RES.todo$Canal,labels=canales$Etiqueta)

parche = character(1*30/dur.chunk)
for(i in 1:(1*30/dur.chunk)){
  parche[i] = toString(i)
}

for(i in 1:(1*30/dur.chunk)){
  tmp = matrix(0,nrow=n.canales,ncol=1*30/dur.chunk+2)
  tmp[,2+1:(1*30/dur.chunk)] = RES.t[(i-1)*dur.chunk+ 1:(1*30/dur.chunk)]
  
  row.names(tmp) = canales$Etiqueta
  colnames(tmp) = c('Promedio','Des. std.',parche)
  for(ch in 1:n.canales){
    tmp[ch,1] = mean(RES.t[ch,((i-1)*1*30/dur.chunk+1):(i*1*30/dur.chunk)])
    tmp[ch,2] =   sd(RES.t[ch,(((i-1)*1*30/dur.chunk+1):(i*1*30/dur.chunk))])
  }
  write.xlsx(tmp,file=archivo.excel,sheetName=paste('pre-MOR',toString(i)),
             col.names=TRUE,row.names=TRUE,
             append=TRUE)
}
for(i in 11:i.d){
  tmp = matrix(0,nrow=n.canales,ncol=1*30/dur.chunk+2)
  tmp[,2+1:(1*30/dur.chunk)] = RES.t[(i-1)*dur.chunk+ 1:(1*30/dur.chunk)]
  
  row.names(tmp) = canales$Etiqueta
  colnames(tmp) = c('Promedio','Des. std.',parche)
  for(ch in 1:n.canales){
    tmp[ch,1] = mean(RES.t[ch,((i-1)*1*30/dur.chunk+1):(i*1*30/dur.chunk)])
    tmp[ch,2] =   sd(RES.t[ch,(((i-1)*1*30/dur.chunk+1):(i*1*30/dur.chunk))])
  }
  write.xlsx(tmp,file=archivo.excel,sheetName=paste('MOR',toString(i)),
             col.names=TRUE,row.names=TRUE,
             append=TRUE)
}

setwd(dir_graf)

###############################################################################
###############################################################################

setwd(dir_graf)

archivo.csv = paste0('estacionariedad_',nombre,'_',toString(dur.chunk),'.csv')

write.csv(RES.todo,file=archivo.csv,row.names=F)

###############################################################################
archivo.csv2 = paste0('estacionariedad_',nombre,'_',toString(dur.chunk),
                      '_promedios.csv')

RES.suma$Proporcion = RES.suma$Epocas_estacionarias/RES.suma$Epocas

write.csv(RES.suma,file=archivo.csv2,row.names=F)

###############################################################################

require(ggpubr)

RES.todo$Etapa = factor(RES.todo$Etapa,labels=c('NMOR','MOR'))
RES.todo$Indice = RES.todo$Indice - (RES.todo$Etapa=='MOR')*10*30/dur.chunk

print(
ggplot(RES.todo,aes(x=Indice,y=Canal,fill=Estacionario)) + 
  #scale_x_continuous(breaks = pretty((indice.epocas-indice.epocas[1])*30/factor.extra, n = 20),
  #                   expand=c(0,0)) +
  scale_fill_gradient2(low = 'white',high='black',midpoint=.5)+
  labs(title=etiqueta,
       subtitle='Épocas que son estacionarias')+
  geom_raster()+
  #geom_vline(xintercept=10*30/factor.extra)
  facet_grid(.~Etapa) +
  scale_x_continuous(expand=c(0,0))
)
print(
ggplot(RES.todo,aes(x=Canal,y=Estacionario,fill=Etapa)) + 
  labs(title=etiqueta,
       subtitle='Comparación MOR vs NMOR para estacionariedad')+
  geom_boxplot()+
  theme(legend.position='bottom')+
  stat_compare_means(label = 'p.signif',method='t.test')+
  theme(axis.title.x=element_blank())+
  rotate_x_text(angle = 45)
)
#################################################