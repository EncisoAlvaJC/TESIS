#################################################
# parametros dependientes de los datos
n_canales = length(kanales$Etiqueta)
ventana   = dur_epoca*fr_muestreo

#################################################
# procesamiento parametros opcionales (ajuste)
if(unidad_par_t =='tiempo'){
  ini_t   = hms2t(ajuste_ini_hms) 
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

min_e = 1

#################################################
# optimizacion: lee el tamano de los datos el contenedor
setwd(dir_res_mid)
ch            = 1
ch_actual     = kanales$Nombre_archivo[ch]
nom_arch      = paste0('EST_',nombre,'_',ch_actual,
                       '_T_',toString(dur_epoca),'.txt')
pv_t          = scan(nom_arch)
pv_t          = as.numeric(t(pv_t))

factor_escala = dur_epoca/30
n_epocas      = length(pv_t)
max_e         = n_epocas

#################################################
# zoom
max_e = hms2t(max_hms)/dur_epoca

#################################################
# contenedores de los datos
RES_T   = matrix(0,nrow=n_canales,ncol=max_e)
RES_TIR = matrix(0,nrow=n_canales,ncol=max_e)

#################################################
# inicio ciclo que recorre todos los canales
for(ch in 1:n_canales){
  # cargar los datos
  ch_actual = kanales$Nombre_archivo[ch]
  nom_arch  = paste0('EST_',nombre,'_',ch_actual,
                     '_T_'  ,toString(dur_epoca),'.txt')
  pv_t      = scan(nom_arch)
  pv_t      = as.numeric(t(pv_t))
  
  nom_arch  = paste0('EST_',nombre,'_',ch_actual,
                     '_TIR_',toString(dur_epoca),'.txt')
  pv_tir    = scan(nom_arch)
  pv_tir    = as.numeric(t(pv_tir))
  
  mmm1 = min(n_epocas,length(pv_t),max_e)
  mmm2 = min(n_epocas,length(pv_tir),max_e)
  
  # organizacion de los datos en una matriz
  RES_T[ch,1:mmm1]   = pv_t[1:mmm1]
  RES_TIR[ch,1:mmm2] = pv_tir[1:mmm2]
}
# fin ciclo que recorre canales
#################################################

# pedazo final de la prueba de PSR
if(binario){
  RES_T[  is.na(RES_T  )] = 1
  RES_TIR[is.na(RES_TIR)] = 1
  
  M_RES1 = 1*( RES_TIR<.05 )
  M_RES2 = 1*( RES_T  <.05 )
  M_RES = pmin(M_RES1,M_RES2)
  RES_T = (-M_RES+1)*1
  
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

RES_MOR = RES_T[,indice]

#################################################
# V de Cramer

corr2 = matrix(NA,ncol=3,nrow=22**2)

for(i in 1:22){
  for(j in 1:22){
    corr2[22*(i-1) + j,] = c(i,j,cv.test(RES_T[i,],RES_T[j,]))
  }
}

corr2 = as.data.frame(corr2)
colnames(corr2) = c('i','j','V')

for(k in 1:length(corr2$i)){
  if(corr2$i[k]>=corr2$j[k]){
    corr2$V[k] = NA
  }
}

corr2$i = factor(corr2$i,labels=kanales$Etiqueta)
corr2$j = factor(corr2$j,labels=kanales$Etiqueta)

corr3 = corr2[!is.na(corr2$V),]

print(etiqueta)

A = ggplot(corr3,aes(x=i,y=j,fill=V)) +
  theme_bw() +
  xlab(NULL) + ylab(NULL) +
  ggtitle(paste(etiqueta,' | Total')) +
  scale_fill_distiller(palette='Spectral',limits=c(1,-1)) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  #theme(legend.position = 'left') +
  geom_raster()
plot(A)

corr2$V[is.na(corr2$V)] = 0

ccc = dcast(corr2,i~j)
View(ccc)
invisible(readline(prompt="Presion [enter] para continuar"))

#################################################
# V de Cramer

RES_MOR[,1] = 1

corr2 = matrix(NA,ncol=3,nrow=22**2)

for(i in 1:22){
  for(j in 1:22){
    corr2[22*(i-1) + j,] = c(i,j,cv.test(RES_MOR[i,],RES_MOR[j,]))
  }
}

corr2 = as.data.frame(corr2)
colnames(corr2) = c('i','j','V')

for(k in 1:length(corr2$i)){
  if(corr2$i[k]>=corr2$j[k]){
    corr2$V[k] = NA
  }
}

corr2$i = factor(corr2$i,labels=kanales$Etiqueta)
corr2$j = factor(corr2$j,labels=kanales$Etiqueta)

corr3 = corr2[!is.na(corr2$V),]

print(etiqueta)

A = ggplot(corr3,aes(x=i,y=j,fill=V)) +
  theme_bw() +
  xlab(NULL) + ylab(NULL) +
  ggtitle(paste(etiqueta,' | MOR')) +
  scale_fill_distiller(palette='Spectral',limits=c(1,-1)) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  #theme(legend.position = 'left') +
  geom_raster()
plot(A)

corr2$V[is.na(corr2$V)] = 0

ccc = dcast(corr2,i~j)
View(ccc)
invisible(readline(prompt="Presion [enter] para continuar"))

# fin grafico
#################################################