###################################################################################################
###################################################################################################
###################################################################################################

psr_canal = function(nombre,
                     extension   = '.txt',
                     canales     = '10-20',
                     fr_muestreo = 512,
                     dur_epoca   = 4,
                     ver_avance  = F,
                     no_repetir  = F,
                     etiqueta,
                     dir_datos,
                     dir_res,
                     haz_carpeta = F,
                     usar_loess  = F)
{
  #################################################
  # libreria que contiene la prueba de PSR
  require('fractal')
  
  #################################################
  # revisar si faltan algunos parametros
  if(missing(nombre)){
    stop('ERROR: Especifique el nombre de los archivos de registro.')
  }
  if(missing(canales)){
    stop('ERROR: Especifique los canales que seran analizados.')
  }
  if(missing(fr_muestreo)){
    warning('WARNING: No se ha indicado la frecuencia de muestreo. La escala de tiempo puede ser inadecuada')
  }
  if(missing(dur_epoca)){
    warning('WARNING: No se ha indicado el tamano de la epoca; revise el texto para mas detalles sobre los posibles efectos de esta advertencia.')
  }
  
  #################################################
  # parametros opcionales
  if(canales=='10-20'){
    canales = c('C3','C4','CZ','F3','F4','F7','F8','FP1','FP2','FZ','O1','O2',
                'P3','P4','PZ','ROG','T3','T4','T5','T6')
  }
  if(canales=='PSG'){
    canales = c('C3','C4','CZ','F3','F4','F7','F8','FP1','FP2','FZ','O1','O2',
                'P3','P4','PZ','ROG','T3','T4','T5','T6','LOG','ROG','EMG')
  }
  if(!is.list(canales)){
    stop('ERROR: Lista de canales no valida. Por favor, revise la sintaxis en la documentacion.')
  }
  if(length(canales)<1){
    stop('ERROR: Lista de canales tiene longitud cero. Por favor, revise la sintaxis en la documentacion.')
  }
  if(missing(dir_datos)){
    dir_datos = getwd()
  }else{
    if(!dir.exists(dir_datos)){
      stop('ERROR: El directorio especificado con datos no existe, o no puede ser leido.')
    }
  }
  if(missing(dir_res)){
    if(haz_carpeta){
      dir_res = paste0(getwd(),'/est_',nombre)
      dir.create(dir_res)
    }else{
      dir_res = getwd()
    }
  }else{
    if(!dir.exists(dir_res)){
      warning('ERROR: El directorio especificado para guardar los resultados no existe, o no puede ser leido. Se usara el directorio de datos.')
    }
  }
  if(missing(etiqueta)){
    etiqueta = nombre
  }
  
  # parametros dependientes de los datos
  ventana   = fr_muestreo*dur_epoca
  n_canales = length(canales)
  usar_stl  = T
  if(dur_epoca<=2){
    usar_stl = F
  }
  if(usar_loess){
    usar_stl = F
  }
  
  #################################################
  # incicio del ciclo que recorre los canales
  for(ch in 1:n_canales){
    
    # construye el nombre del archivo
    ch_actual  = canales[ch]
    nom_arhivo = paste0(nombre,'_',ch_actual,extension)
    
    if(no_repetir){
      setwd(dir_res)
      if(file.exists(paste0('EST_',nombre,'_',ch_actual,'_T.csv'  ))){
        warning('El canal ',ch_actual,' se ha omitido manualmente, pues se encontraron resultados previos para el mismo.')
        next()
      }
    }
    
    # cargar los datos
    setwd(dir_datos)
    if(!file.exists(nom_arhivo)){
      warning(paste0('ERROR: En canal ',ch_actual,', no se encontro el archivo ',nom_arhivo))
      next()
    }
    DATOS = read.csv(nom_archivo)
    DATOS = as.numeric(unlist(DATOS))
    
    # cuantas epocas pueden formarse
    max_epoca = floor(length(DATOS)/dur_epoca)
    if(max_epoca==0){
      warning(paste0('ERROR: En canal ',ch_actual,', no se pudieron leer datos.'))
      next()
    }
    
    # contenedores de los resltados
    pv.t   = rep(0,max_epoca)
    pv.ir  = rep(0,max_epoca)
    pv.tir = rep(0,max_epoca)
    
    #informacion sobre el progreso, si fue requerida
    if(ver_avance){
      print( paste0('  Sujeto : ',etiqueta) )
      print( paste0('   Canal : ',ch_actual,' (',toString(ch),'/',toString(n_canales),')') )
    }
    
    #################################################
    # inicio del ciclo que recorre las epocas
    for ( i in 0:(max_epoca-1) ){
      
      # filtro STL, robusto y forzado a periodico estandar
      tmp   = DATOS[ (i*ventana+1) : ((i+1)*ventana) ]
      tmp.t = ts(tmp,frequency=fr_muestreo,start=c(0,0))
      if(usar_stl){
        tmp.s = stl(tmp.t,robust=T,s.window='periodic')
        tmp.r = s$time.series[,'remainder']
        tmp   = as.numeric(unclass(tmp.r))
      }else{
        tmp.l = loess(tmp~time(tmp.t))
        tmp.s = predict(tmp.l,time(tmp.t))
        tmp   = tmp - tmp.s$fit
      }
      
      # test de PSR, los archivos se recolectan
      z         = stationarity(tmp)
      pv.t[i]   = as.numeric( attr(z,'pvals')[1])
      pv.ir[i]  = as.numeric( attr(z,'pvals')[2])
      pv.tir[i] = as.numeric( attr(z,'pvals')[3])
    }
    # fin del ciclo que recorre las epocas
    #################################################
    
    # los resultados se guardan en un archivo .csv
    setwd(dir_res)
    write.csv(pv.t  , paste0('EST_',nombre,'_',ch_actual,'_T.csv'  ))
    #write.csv(pv.ir , paste0('EST_',nombre,'_',ch_actual,'_IR.csv' ))
    #write.csv(pv.tir, paste0('EST_',nombre,'_',ch_actual,'_TIR.csv'))
  }
  # fin del ciclo que recorre canales
  #################################################
}

###############################################################################
###############################################################################
###############################################################################