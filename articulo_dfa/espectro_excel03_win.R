###############################################################################
# parametros
grabar         = FALSE
potencia.total = T
#sujeto         = 2
orden_stam     = TRUE

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
dir_epocas  = 'C:/Users/EQUIPO 1/Desktop/julio/epocas_dfa_10'
dir_graf    = 'C:/Users/EQUIPO 1/Desktop/julio/TESIS/articulo_dfa/espectro_extendido'

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
info     = read_excel(paste0(dir_info,'/info_participantes.xlsx'))
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

epo0 = indice.epocas[1]
indice.epocas = unique(c((epo0-10):epo0 , indice.epocas ))

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
colnames(gral) = c('MOR [#]','Epoca [#]','Inicio [hhmmss]','Fin [hhmmss]',
                   'Etapa')

for(i in 1:length(indice.epocas)){
  hhmmss1  = t2hms((indice.epocas[i]-1)*30/factor.extra)
  hhmmss2  = corregir.hms(hhmmss1+c(0,0,30/factor.extra))
  gral[i,] = c(i-9,indice.epocas[i],hms2txt(hhmmss1),hms2txt(hhmmss2),'')
}

gral[,1] = rep(1:10,2)
gral[,5] = c(rep(0,10),rep(1,10))

# 0 = NMOR
# 1 =  MOR

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

RES.todo = as.data.frame(matrix(0,ncol=4+n.bandas+2,nrow=n.canales*i.d))
colnames(RES.todo) = c('Participante','Canal','EpocaMOR','NumEpoca',
                       'Delta','Theta','Alfa','Beta','Gamma','Varianza',
                       'Ratio')

RES.todo[,1] = rep(sujeto,n.canales*i.d)

archivo.csv = paste0('espectro',nombre,'_',agregado,'.csv')

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
    
    if(potencia.total){
      n.archivo = paste0('VAR_',nombre,'_',ch.actual,'.txt')
      potencia  = scan(n.archivo)
      
      if(quitar.artefactos){
        q3  = quantile(potencia,.75,na.rm=T)
        q1  = quantile(potencia,.25,na.rm=T)
        ric = 2*(q3-q1)
        q3  = q3 + ric
        q1  = q1 - ric
        
        potencia = pmin(potencia,q3)
        potencia = pmax(potencia,q1)
      }
      
      datos = datos*potencia
      
      if(quitar.artefactos){
        q3  = quantile(datos,.75,na.rm=T)
        q1  = quantile(datos,.25,na.rm=T)
        ric = 2*(q3-q1)
        q3  = q3 + ric
        q1  = q1 - ric
        
        datos = pmin(datos,q3)
        datos = pmax(datos,q1)
      }
    }
    
    if(zoom){
      datos    = datos[indice.chunk]
    }
    
    # organizacion de los datos en una matriz
    RES[ch,] = datos
  }
  # fin ciclo que recorre canales
  #################################################
  
  setwd(dir_graf)
  
  for(ch in 1:n.canales){
    step.ch  = i.d*(ch-1)
    
    RES.todo[step.ch+(1:i.d),banda.actual+4] = RES[ch,]
    RES.todo[step.ch+1:i.d,2] = rep(ch,i.d)
  }
  
  RES[is.na(RES)*1==1] = 0
  
  parche = character(30/factor.extra)
  for(i in 1:(30/factor.extra)){
    parche[i] = toString(i)
  }
  
  for(i in 1:(length(indice.epocas)/2)){
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
  
  for(i in (length(indice.epocas)/2+1):(length(indice.epocas))){
    tmp = matrix(0,nrow=n.canales,ncol=30/factor.extra+2)
    row.names(tmp) = canales$Etiqueta
    colnames(tmp) = c('Promedio','Des. std.',parche)
    for(ch in 1:n.canales){
      tmp[ch,1] = mean(RES[ch,(((i-1)*30/factor.extra+1):(i*30/factor.extra))])
      tmp[ch,2] =   sd(RES[ch,(((i-1)*30/factor.extra+1):(i*30/factor.extra))])
    }
    tmp[,1:(30/factor.extra)+2] = 
      RES[,(((i-1)*30/factor.extra+1):(i*30/factor.extra))]
    write.xlsx(tmp,file=archivo.excel,sheetName=paste('MOR',toString(i-10)),
               col.names=TRUE,row.names=TRUE,
               append=TRUE)
  }
  
  setwd(dir_graf)
  
  # DEBUG
  #stop()
}

suma = RES.todo$Delta + RES.todo$Theta + RES.todo$Alfa + 
  RES.todo$Beta + RES.todo$Gamma

suma[suma==0]  = 1

RES.todo$Delta = RES.todo$Delta/suma
RES.todo$Theta = RES.todo$Theta/suma
RES.todo$Alfa  = RES.todo$Alfa/suma
RES.todo$Beta  = RES.todo$Beta/suma
RES.todo$Gamma = RES.todo$Gamma/suma





{
  setwd(dir_res_pre)
  
  n.archivo = paste0('VAR_',nombre,'_',ch.actual,'.txt')
  potencia  = scan(n.archivo)
  
  # for(i in 1:3){
  #   tope     = quantile(potencia,.95,na.rm = T)
  #   potencia = pmin(potencia,tope)
  # }
  
  #if(quitar.artefactos){
  #  q3  = quantile(potencia,.75,na.rm=T)
  #  q1  = quantile(potencia,.25,na.rm=T)
  #  ric = 2*(q3-q1)
  #  q3  = q3 + ric
  #  q1  = q1 - ric
  #  
  #  potencia = pmin(potencia,q3)
  #  potencia = pmax(potencia,q1)
  #}
  
  #RES.todo$Varianza = potencia
  
  ab        = RES.todo$Alfa + RES.todo$Beta
  ab[ab<=0] = 1
  
  RES.todo$Ratio = (RES.todo$Delta + RES.todo$Theta)/ab
  
  #if(potencia.total){
  #  RES.todo$Delta = RES.todo$Delta*potencia
  #  RES.todo$Theta = RES.todo$Theta*potencia
  #  RES.todo$Alfa  = RES.todo$Alfa*potencia
  #  RES.todo$Beta  = RES.todo$Beta*potencia
  #  RES.todo$Gamma = RES.todo$Gamma*potencia
  #}
}

{
  archivo.excel = paste0(nombre,'_','Varianza',
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
    n.archivo = paste0('VAR_',nombre,'_',ch.actual,'.txt')
    datos     = scan(n.archivo)
    
    if(zoom){
      datos    = datos[indice.chunk]
    }
    
    # organizacion de los datos en una matriz
    RES[ch,] = datos
  }
  
  for(ch in 1:n.canales){
    step.ch  = i.d*(ch-1)
    
    RES.todo[step.ch+(1:i.d),10] = RES[ch,]
  }
  
  RES[is.na(RES)*1==1] = 0
  # fin ciclo que recorre canales
  #################################################
  
  setwd(dir_graf)
  
  parche = character(30/factor.extra)
  for(i in 1:(30/factor.extra)){
    parche[i] = toString(i)
  }
  
  for(i in 1:(length(indice.epocas)/2)){
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
  for(i in (length(indice.epocas)/2+1):(length(indice.epocas))){
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

#RES.todo$Canal = factor(RES.todo$Canal,labels=canales$Etiqueta)

setwd(dir_graf)

write.csv(RES.todo,file=archivo.csv,row.names=F)

# ggplot(RES.todo,aes(x=Canal,y=Delta)) + 
#   #geom_boxplot(notch = T) +
#   geom_boxplot() +
#   xlab(NULL) + ylab(bandas$Banda[1]) +
#   labs(title=paste0(etiqueta,' | ',bandas$Banda[1]))
# ggplot(RES.todo,aes(x=Canal,y=Theta)) +
#   #geom_boxplot(notch = T) +
#   geom_boxplot() +
#   xlab(NULL) + ylab(bandas$Banda[2]) +
#   labs(title=paste0(etiqueta,' | ',bandas$Banda[2]))
# ggplot(RES.todo,aes(x=Canal,y=Alfa)) +
#   #geom_boxplot(notch = T) +
#   geom_boxplot() +
#   xlab(NULL) + ylab(bandas$Banda[3]) +
#   labs(title=paste0(etiqueta,' | ',bandas$Banda[3]))
# ggplot(RES.todo,aes(x=Canal,y=Beta)) +
#   #geom_boxplot(notch = T) +
#   geom_boxplot() +
#   xlab(NULL) + ylab(bandas$Banda[4]) +
#   labs(title=paste0(etiqueta,' | ',bandas$Banda[4]))
# ggplot(RES.todo,aes(x=Canal,y=Gamma)) +
#   #geom_boxplot(notch = T) +
#   geom_boxplot() +
#   xlab(NULL) + ylab(bandas$Banda[5]) +
#   labs(title=paste0(etiqueta,' | ',bandas$Banda[5]))
#multiplot(p1,p2,p3,p4,p5)
#################################################










# multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
#   library(grid)
#   
#   # Make a list from the ... arguments and plotlist
#   plots <- c(list(...), plotlist)
#   
#   numPlots = length(plots)
#   
#   # If layout is NULL, then use 'cols' to determine layout
#   if (is.null(layout)) {
#     # Make the panel
#     # ncol: Number of columns of plots
#     # nrow: Number of rows needed, calculated from # of cols
#     layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
#                      ncol = cols, nrow = ceiling(numPlots/cols))
#   }
#   
#   if (numPlots==1) {
#     print(plots[[1]])
#     
#   } else {
#     # Set up the page
#     grid.newpage()
#     pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
#     
#     # Make each plot, in the correct location
#     for (i in 1:numPlots) {
#       # Get the i,j matrix positions of the regions that contain this subplot
#       matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
#       
#       print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
#                                       layout.pos.col = matchidx$col))
#     }
#   }
# }

#######################################################################
#######################################################################
#######################################################################

if(FALSE){
  
  potencia.total = T
  
  require(readr)
  #require(ggsignif)
  require(ggpubr)
  
  sujetos = 1:5
  gpo = 'CTL'
  #sujetos = c(6:9)
  #gpo = 'PDC'
  
  if(potencia.total){
    agregado = 'total'
  }else{
    agregado = 'relativo'
  }
  
  RES.nuevo = c()
  
  for(sujeto in sujetos){
    tt = read_csv(paste0(dir_graf,'/espectro',info$Nombre_archivo[sujeto],
                         '_',agregado,'.csv'))
    RES.nuevo = rbind(RES.nuevo,tt)
  }
  
  RES.nuevo$Participante = factor(RES.nuevo$Participante,
                                  labels = info$Etiqueta[sujetos])
  RES.nuevo$Etapa        = factor(RES.nuevo$Etapa,
                                  labels = c('NMOR','MOR'))
  
  RES.nuevo$Canal        = factor(RES.nuevo$Canal,
                                  labels=canales$Etiqueta)
  
  ggplot(RES.nuevo,aes(x=Canal,y=Delta,fill=Participante)) +
    #geom_boxplot(notch = T) +
    geom_boxplot() +
    xlab(NULL) + ylab('햞ea bajo la curva') +
    labs(title=paste('Grupo',gpo,'| Ondas Delta (',agregado,')')) +
    theme(legend.position='bottom') +
    #geom_signif(comparisons = list(c('MJH','JAE')))
    stat_compare_means(label = 'p.signif',method='kruskal.test')+
    rotate_x_text(angle = 45)
  
  ggplot(RES.nuevo,aes(x=Canal,y=Theta,fill=Participante)) +
    #geom_boxplot(notch = T) +
    geom_boxplot() +
    xlab(NULL) + ylab('햞ea bajo la curva') +
    labs(title=paste('Grupo',gpo,'| Ondas Theta (',agregado,')')) +
    theme(legend.position='bottom')+
    stat_compare_means(label = 'p.signif',method='kruskal.test')+
    rotate_x_text(angle = 45)
  
  ggplot(RES.nuevo,aes(x=Canal,y=Alfa,fill=Participante)) +
    #geom_boxplot(notch = T) +
    geom_boxplot() +
    xlab(NULL) + ylab('햞ea bajo la curva') +
    labs(title=paste('Grupo',gpo,'| Ondas Alfa (',agregado,')')) +
    theme(legend.position='bottom')+
    stat_compare_means(label = 'p.signif',method='kruskal.test')+
    rotate_x_text(angle = 45)
  
  ggplot(RES.nuevo,aes(x=Canal,y=Beta,fill=Participante)) +
    #geom_boxplot(notch = T) +
    geom_boxplot() +
    xlab(NULL) + ylab('햞ea bajo la curva') +
    labs(title=paste('Grupo',gpo,'| Ondas Beta (',agregado,')')) +
    theme(legend.position='bottom')+
    stat_compare_means(label = 'p.signif',method='kruskal.test')+
    rotate_x_text(angle = 45)
  
  ggplot(RES.nuevo,aes(x=Canal,y=Gamma,fill=Participante)) +
    #geom_boxplot(notch = T) +
    geom_boxplot() +
    xlab(NULL) + ylab('햞ea bajo la curva') +
    labs(title=paste('Grupo',gpo,'| Ondas Gamma (',agregado,')')) +
    theme(legend.position='bottom')+
    stat_compare_means(label = 'p.signif',method='kruskal.test')+
    rotate_x_text(angle = 45)
  
  ggplot(RES.nuevo,aes(x=Canal,y=Varianza,fill=Participante)) +
    #geom_boxplot(notch = T) +
    geom_boxplot() +
    xlab(NULL) + ylab('햞ea bajo la curva') +
    labs(title=paste('Grupo',gpo,'| Potencia total')) +
    theme(legend.position='bottom')+
    stat_compare_means(label = 'p.signif',method='kruskal.test')+
    rotate_x_text(angle = 45)
  
  ggplot(RES.nuevo,aes(x=Canal,y=Ratio,fill=Participante)) +
    #geom_boxplot(notch = T) +
    geom_boxplot() +
    xlab(NULL) + ylab('햞ea bajo la curva') +
    labs(title=paste('Grupo',gpo,'| Proporcion enlentecimiento (',
                     agregado,')')) +
    theme(legend.position='bottom')+
    stat_compare_means(label = 'p.signif',method='kruskal.test')+
    rotate_x_text(angle = 45)
}
