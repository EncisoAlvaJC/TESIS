###############################################################################
# parametros
potencia.total = F
orden_stam     = TRUE

# tamano de la ventana analizada, nombre raro para evitar confusiones
dur.chunk      = 1

quitar.artefactos = TRUE

zoom           = T
unidad_par_t   = 'tiempo'

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
dir_scripts = 'C:/Users/EQUIPO 1/Desktop/julio/TESIS/articulo_dfa/scripts'
dir_res_pre = 'C:/Users/EQUIPO 1/Desktop/julio/espectro_integrado_171104'
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

ajuste_ini_hms = c(info$ini_hh[sujeto],info$ini_mm[sujeto],info$ini_ss[sujeto])
min_hms        = c(info$ini_hh[sujeto],info$ini_mm[sujeto],info$ini_ss[sujeto])
max_hms        = c(info$fin_hh[sujeto],info$fin_mm[sujeto],info$fin_ss[sujeto])

grupo = info$Grupo_n[sujeto]
if(grupo== 0){gruppo = 'CTRL'}
if(grupo== 1){gruppo = 'PMCI'}
if(grupo==-1){gruppo = 'EXCL'}

#################################################
# ajustes del zoom
if(zoom){
  validar.zoom = F
  if(unidad_par_t=='tiempo'){
    s.ini = hms2t(min_hms)
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
indice.chunk.t = c()
for(k in 1:(30/factor.extra)){
  indice.chunk.t = c(indice.chunk.t,indice.chunk+k)
}
indice.chunk = sort(unique(indice.chunk.t))

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

if(potencia.total){
  agregado = 'total'
}else{
  agregado = 'relativo'
}

#################################################
# optimizacion: lee el tamano de los datos el contenedor
setwd(dir_res_pre)
ch        = 19
ch.actual = canales$Nombre_archivo[ch]
n.archivo = paste0('SP_INT_',nombre,'_',ch.actual,'_TOTAL.txt')
datos     = scan(n.archivo)
n.chunks  = length(datos)

indice.chunk = indice.chunk[indice.chunk<=n.chunks]

i.d = length(indice.chunk)

RES.todo = as.data.frame(matrix(0,ncol=8+n.bandas,nrow=n.canales*i.d))
colnames(RES.todo) = c('Participante','Canal','EpocaMOR','NumEpoca',
                       'Delta','Theta','Alfa','Beta','Gamma','Varianza',
                       'Grupo','Etapa','Indice')

#RES.todo[,1] = rep(nombre,n.canales*i.d)
RES.todo[,1] = rep(sujeto,n.canales*i.d)
if(sujeto<6){
  RES.todo[,11] = rep(0,n.canales*i.d)
}else{
  RES.todo[,11] = rep(1,n.canales*i.d)
}

archivo.csv = paste0('espectro',nombre,'_',agregado,'.csv')

indize = 1:i.d

for(banda.actual in 1:n.bandas){
  
  #################################################
  # contenedores de los datos
  RES = matrix(0,nrow=n.canales,ncol=length(indice.chunk))
  row.names(RES) = canales$Etiqueta
  
  #################################################
  # inicio ciclo que recorre todos los canales
  for(ch in 1:n.canales){
    setwd(dir_res_pre)
    
    # cargar los datos
    ch.actual = canales$Nombre_archivo[ch]
    n.archivo = paste0('SP_INT_',nombre,'_',ch.actual,'_',
                       bandas$Nombre_archivo[banda.actual],'.txt')
    datos     = scan(n.archivo)

    if(zoom){
      datos    = datos[indice.chunk]
    }
    
    # organizacion de los datos en una matriz
    RES[ch,] = datos
  }
  # fin ciclo que recorre canales
  #################################################
  
  #setwd(dir_graf)
  
  for(ch in 1:n.canales){
    setwd(dir_res_pre)
    
    step.ch  = i.d*(ch-1)
    
    RES.todo[step.ch+(1:i.d),banda.actual+4] = RES[ch,]
    RES.todo[step.ch+1:i.d,2] = rep(ch,i.d)
    
    RES.todo[step.ch+1:i.d,13] = indize
    
    RES.todo[step.ch+(1:(10*30/factor.extra)),12]   = 
      rep(0,10*30/factor.extra)
    RES.todo[step.ch+((10*30/factor.extra+1):i.d),12] = 
      rep(1,i.d-10*30/factor.extra)
  }

  #RES[is.na(RES)*1==1] = 0
  
  setwd(dir_graf)
}

###############################################################################
suma = RES.todo$Delta + RES.todo$Theta + RES.todo$Alfa + 
  RES.todo$Beta + RES.todo$Gamma

RES.todo$Delta = RES.todo$Delta/suma
RES.todo$Theta = RES.todo$Theta/suma
RES.todo$Alfa  = RES.todo$Alfa/suma
RES.todo$Beta  = RES.todo$Beta/suma
RES.todo$Gamma = RES.todo$Gamma/suma

if(potencia.total){
  setwd(dir_res_pre)
  
  n.archivo = paste0('VAR_',nombre,'_',ch.actual,'.txt')
  potencia  = scan(n.archivo)
  potencia  = potencia[indice.chunk]
  
  RES.todo$Delta = RES.todo$Delta*potencia
  RES.todo$Theta = RES.todo$Theta*potencia
  RES.todo$Alfa  = RES.todo$Alfa*potencia
  RES.todo$Beta  = RES.todo$Beta*potencia
  RES.todo$Gamma = RES.todo$Gamma*potencia
  
  kk = 3
  
  if(quitar.artefactos){
    q1  = quantile(RES.todo$Delta,.25,na.rm=T)
    q3  = quantile(RES.todo$Delta,.75,na.rm=T)
    riq = kk*(q3-q1)
    RES.todo$Delta = pmin(RES.todo$Delta,q3+riq)
    RES.todo$Delta = pmax(RES.todo$Delta,q1-riq)
    
    q1  = quantile(RES.todo$Theta,.25,na.rm=T)
    q3  = quantile(RES.todo$Theta,.75,na.rm=T)
    riq = kk*(q3-q1)
    RES.todo$Theta = pmin(RES.todo$Theta,q3+riq)
    RES.todo$Theta = pmax(RES.todo$Theta,q1-riq)
    
    q1  = quantile(RES.todo$Alfa,.25,na.rm=T)
    q3  = quantile(RES.todo$Alfa,.75,na.rm=T)
    riq = kk*(q3-q1)
    RES.todo$Alfa = pmin(RES.todo$Alfa,q3+riq)
    RES.todo$Alfa = pmax(RES.todo$Alfa,q1-riq)
    
    q1  = quantile(RES.todo$Beta,.25,na.rm=T)
    q3  = quantile(RES.todo$Beta,.75,na.rm=T)
    riq = kk*(q3-q1)
    RES.todo$Beta = pmin(RES.todo$Beta,q3+riq)
    RES.todo$Beta = pmax(RES.todo$Beta,q1-riq)
    
    q1  = quantile(RES.todo$Gamma,.25,na.rm=T)
    q3  = quantile(RES.todo$Gamma,.75,na.rm=T)
    riq = kk*(q3-q1)
    RES.todo$Gamma = pmin(RES.todo$Gamma,q3+riq)
    RES.todo$Gamma = pmax(RES.todo$Gamma,q1-riq)
  }
}
###############################################################################

for(banda.actual in 1:n.bandas){
  setwd(dir_graf)
  
  archivo.excel = paste0(nombre,'_',bandas$Nombre_archivo[banda.actual],
                         '_',agregado,'.xlsx')
  
  write.xlsx(tag,file=archivo.excel,
             row.names=FALSE,col.names=FALSE,
             sheetName='General',append=FALSE,showNA=TRUE)
  write.xlsx(gral,file=archivo.excel,
             row.names=FALSE,col.names=TRUE,
             sheetName='Epocas',append=TRUE,showNA=TRUE)
  
  setwd(dir_graf)
  
  #RES[is.na(RES)*1==1] = 0
  
  RES = matrix(0,nrow=n.canales,ncol=length(indice.chunk))
  row.names(RES) = canales$Etiqueta
  
  RES.todo$Canal = factor(RES.todo$Canal,labels=canales$Etiqueta)
  
  for(ch in 1:n.canales){
    k        = RES.todo[grep(canales$Etiqueta[ch],RES.todo$Canal),]
    RES[ch,] = k[,bandas$Banda[banda.actual]]
  }
  
  
  parche = character(30/factor.extra)
  for(i in 1:(30/factor.extra)){
    parche[i] = toString(i)
  }
  
  for(i in 1:10){
    tmp = matrix(0,nrow=n.canales,ncol=30/factor.extra+2)
    row.names(tmp) = canales$Etiqueta
    colnames(tmp) = c('Promedio','Des. std.',parche)
    for(ch in 1:n.canales){
      tmp[ch,1] = mean(RES[ch,(((i-1)*30/factor.extra+1):(i*30/factor.extra))])
      tmp[ch,2] =   sd(RES[ch,(((i-1)*30/factor.extra+1):(i*30/factor.extra))])
    }
    tmp[,1:(30/factor.extra)+2] = 
      RES[,(((i-1)*30/factor.extra+1):(i*30/factor.extra))]
    write.xlsx(tmp,file=archivo.excel,sheetName=paste('pre-MOR',toString(i)),
               col.names=TRUE,row.names=TRUE,
               append=TRUE)
  }
  for(i in 11:(length(indice.epocas))){
    tmp = matrix(0,nrow=n.canales,ncol=30/factor.extra+2)
    row.names(tmp) = canales$Etiqueta
    colnames(tmp) = c('Promedio','Des. std.',parche)
    for(ch in 1:n.canales){
      tmp[ch,1] = mean(RES[ch,(((i-1)*30/factor.extra+1):(i*30/factor.extra))])
      tmp[ch,2] =   sd(RES[ch,(((i-1)*30/factor.extra+1):(i*30/factor.extra))])
    }
    tmp[,1:(30/factor.extra)+2] = 
      RES[,(((i-1)*30/factor.extra+1):(i*30/factor.extra))]
    write.xlsx(tmp,file=archivo.excel,sheetName=paste('MOR',toString(i)),
               col.names=TRUE,row.names=TRUE,
               append=TRUE)
  }
  
  setwd(dir_graf)
}

###############################################################################
###############################################################################

{
  archivo.excel = paste0(nombre,'_VARIANZA',
                         '_',agregado,'.xlsx')
  
  write.xlsx(tag,file=archivo.excel,
             row.names=FALSE,col.names=FALSE,
             sheetName='General',append=FALSE,showNA=TRUE)
  write.xlsx(gral,file=archivo.excel,
             row.names=FALSE,col.names=TRUE,
             sheetName='Epocas',append=TRUE,showNA=TRUE)
  
  #################################################
  # contenedores de los datos
  RES = matrix(0,nrow=n.canales,ncol=length(indice.chunk))
  row.names(RES) = canales$Etiqueta
  
  #################################################
  # inicio ciclo que recorre todos los canales
  for(ch in 1:n.canales){
    setwd(dir_res_pre)
    # cargar los datos
    ch.actual = canales$Nombre_archivo[ch]
    n.archivo = paste0('SP_INT_',nombre,'_',ch.actual,'_',
                       bandas$Nombre_archivo[1],'.txt')
    datos     = scan(n.archivo)
    
    if(zoom){
      datos    = datos[indice.chunk]
    }
    
    # organizacion de los datos en una matriz
    RES[ch,] = datos
  }
  
  for(ch in 1:n.canales){
    setwd(dir_res_pre)
    step.ch  = i.d*(ch-1)
    
    RES.todo[step.ch+(1:i.d),10] = RES[ch,]
    #RES.todo[step.ch+1:i.d,2] = rep(ch,i.d)
  }
  
  RES[is.na(RES)*1==1] = 0
  # fin ciclo que recorre canales
  #################################################
  
  setwd(dir_graf)
  
  parche = character(30/factor.extra)
  for(i in 1:(30/factor.extra)){
    parche[i] = toString(i)
  }
  
  for(i in 1:10){
    tmp = matrix(0,nrow=n.canales,ncol=30/factor.extra+2)
    row.names(tmp) = canales$Etiqueta
    colnames(tmp) = c('Promedio','Des. std.',parche)
    for(ch in 1:n.canales){
      tmp[ch,1] = mean(RES[ch,(((i-1)*30/factor.extra+1):(i*30/factor.extra))])
      tmp[ch,2] =   sd(RES[ch,(((i-1)*30/factor.extra+1):(i*30/factor.extra))])
    }
    tmp[,1:(30/factor.extra)+2] = 
      RES[,(((i-1)*30/factor.extra+1):(i*30/factor.extra))]
    write.xlsx(tmp,file=archivo.excel,sheetName=paste('pre-MOR',toString(i)),
               col.names=TRUE,row.names=TRUE,
               append=TRUE)
  }
  for(i in 11:(length(indice.epocas))){
    tmp = matrix(0,nrow=n.canales,ncol=30/factor.extra+2)
    row.names(tmp) = canales$Etiqueta
    colnames(tmp) = c('Promedio','Des. std.',parche)
    for(ch in 1:n.canales){
      tmp[ch,1] = mean(RES[ch,(((i-1)*30/factor.extra+1):(i*30/factor.extra))])
      tmp[ch,2] =   sd(RES[ch,(((i-1)*30/factor.extra+1):(i*30/factor.extra))])
    }
    tmp[,1:(30/factor.extra)+2] = 
      RES[,(((i-1)*30/factor.extra+1):(i*30/factor.extra))]
    write.xlsx(tmp,file=archivo.excel,sheetName=paste('MOR',toString(i)),
               col.names=TRUE,row.names=TRUE,
               append=TRUE)
  }
}

setwd(dir_graf)
write.csv(RES.todo,file=archivo.csv,row.names=F)

###############################################################################
prom0 = RES.todo[grep(0,RES.todo$Etapa),]
mean0 = aggregate(prom0[,c(1,5:12)],
                  list(prom0$Canal),mean)
prom1 = RES.todo[grep(1,RES.todo$Etapa),]
mean1 = aggregate(prom1[,c(1,5:12)],
                  list(prom1$Canal),mean)

promedios = rbind(mean1,mean0)

colnames(promedios)[1] = 'Canal'

archivo.csv2 = paste0('espectro',nombre,'_',agregado,
                      '_promedios.csv')
write.csv(promedios,file=archivo.csv2,row.names=F)

###############################################################################

require(ggpubr)

RES.todo$Etapa = factor(RES.todo$Etapa,labels=c('NMOR','MOR'))

RES.todo$Indice = RES.todo$Indice - (RES.todo$Etapa=='MOR')*10*30/factor.extra

banda.actual=1
print(
ggplot(RES.todo,aes(x=Indice,y=Canal,fill=Delta)) + 
  #scale_x_continuous(breaks = pretty((indice.epocas-indice.epocas[1])*30/factor.extra, n = 20),
  #                   expand=c(0,0)) +
  scale_fill_distiller(palette='Spectral')+
  labs(title=etiqueta,
       subtitle=paste('Espectrograma para la banda',bandas$Banda[banda.actual]))+
  geom_raster()+
  #geom_vline(xintercept=10*30/factor.extra)
  facet_grid(.~Etapa) +
  scale_x_continuous(expand=c(0,0))
)
print(
ggplot(RES.todo,aes(x=Canal,y=Delta,fill=Etapa)) + 
  labs(title=etiqueta,
       subtitle=paste('Comparación MOR vs NMOR para la banda',bandas$Banda[banda.actual]))+
  geom_boxplot()+
  theme(legend.position='bottom')+
  stat_compare_means(label = 'p.signif',method='t.test')+
  theme(axis.title.x=element_blank())+
  rotate_x_text(angle = 45)
)
banda.actual=2
print(
  ggplot(RES.todo,aes(x=Indice,y=Canal,fill=Theta)) + 
    #scale_x_continuous(breaks = pretty((indice.epocas-indice.epocas[1])*30/factor.extra, n = 20),
    #                   expand=c(0,0)) +
    scale_fill_distiller(palette='Spectral')+
    labs(title=etiqueta,
         subtitle=paste('Espectrograma para la banda',bandas$Banda[banda.actual]))+
    geom_raster()+
    #geom_vline(xintercept=10*30/factor.extra)
    facet_grid(.~Etapa) +
    scale_x_continuous(expand=c(0,0))
)
print(
  ggplot(RES.todo,aes(x=Canal,y=Theta,fill=Etapa)) + 
    labs(title=etiqueta,
         subtitle=paste('Comparación MOR vs NMOR para la banda',bandas$Banda[banda.actual]))+
    geom_boxplot()+
    theme(legend.position='bottom')+
    stat_compare_means(label = 'p.signif',method='t.test')+
    theme(axis.title.x=element_blank())+
    rotate_x_text(angle = 45)
)

banda.actual=3
print(
  ggplot(RES.todo,aes(x=Indice,y=Canal,fill=Alfa)) + 
    #scale_x_continuous(breaks = pretty((indice.epocas-indice.epocas[1])*30/factor.extra, n = 20),
    #                   expand=c(0,0)) +
    scale_fill_distiller(palette='Spectral')+
    labs(title=etiqueta,
         subtitle=paste('Espectrograma para la banda',bandas$Banda[banda.actual]))+
    geom_raster()+
    #geom_vline(xintercept=10*30/factor.extra)
    facet_grid(.~Etapa) +
    scale_x_continuous(expand=c(0,0))
)
print(
  ggplot(RES.todo,aes(x=Canal,y=Alfa,fill=Etapa)) + 
    labs(title=etiqueta,
         subtitle=paste('Comparación MOR vs NMOR para la banda',bandas$Banda[banda.actual]))+
    geom_boxplot()+
    theme(legend.position='bottom')+
    stat_compare_means(label = 'p.signif',method='t.test')+
    theme(axis.title.x=element_blank())+
    rotate_x_text(angle = 45)
)

banda.actual=4
print(
  ggplot(RES.todo,aes(x=Indice,y=Canal,fill=Beta)) + 
    #scale_x_continuous(breaks = pretty((indice.epocas-indice.epocas[1])*30/factor.extra, n = 20),
    #                   expand=c(0,0)) +
    scale_fill_distiller(palette='Spectral')+
    labs(title=etiqueta,
         subtitle=paste('Espectrograma para la banda',bandas$Banda[banda.actual]))+
    geom_raster()+
    #geom_vline(xintercept=10*30/factor.extra)
    facet_grid(.~Etapa) +
    scale_x_continuous(expand=c(0,0))
)
print(
  ggplot(RES.todo,aes(x=Canal,y=Beta,fill=Etapa)) + 
    labs(title=etiqueta,
         subtitle=paste('Comparación MOR vs NMOR para la banda',bandas$Banda[banda.actual]))+
    geom_boxplot()+
    theme(legend.position='bottom')+
    stat_compare_means(label = 'p.signif',method='t.test')+
    theme(axis.title.x=element_blank())+
    rotate_x_text(angle = 45)
)
banda.actual=5
print(
  ggplot(RES.todo,aes(x=Indice,y=Canal,fill=Gamma)) + 
    #scale_x_continuous(breaks = pretty((indice.epocas-indice.epocas[1])*30/factor.extra, n = 20),
    #                   expand=c(0,0)) +
    scale_fill_distiller(palette='Spectral')+
    labs(title=etiqueta,
         subtitle=paste('Espectrograma para la banda',bandas$Banda[banda.actual]))+
    geom_raster()+
    #geom_vline(xintercept=10*30/factor.extra)
    facet_grid(.~Etapa) +
    scale_x_continuous(expand=c(0,0))
)
print(
  ggplot(RES.todo,aes(x=Canal,y=Gamma,fill=Etapa)) + 
    labs(title=etiqueta,
         subtitle=paste('Comparación MOR vs NMOR para la banda',bandas$Banda[banda.actual]))+
    geom_boxplot()+
    theme(legend.position='bottom')+
    stat_compare_means(label = 'p.signif',method='t.test')+
    theme(axis.title.x=element_blank())+
    rotate_x_text(angle = 45)
)

ggplot(RES.todo,aes(x=Indice,y=Canal,fill=Gamma)) + 
  #scale_x_continuous(breaks = pretty((indice.epocas-indice.epocas[1])*30/factor.extra, n = 20),
  #                   expand=c(0,0)) +
  scale_fill_distiller(palette='Spectral')+
  labs(title=etiqueta,
       subtitle=paste('Espectrograma para la banda',bandas$Banda[banda.actual]))+
  geom_raster()+
  #geom_vline(xintercept=10*30/factor.extra)+
  facet_grid(.~Etapa) +
  scale_x_continuous(expand=c(0,0))

#################################################