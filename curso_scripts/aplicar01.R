# nombres de los archivos/directorios con los datos
nomb_dir  = c('VCNNS',
              'MJNNVIGILOScCanal',
              'JANASUE_revisado',
              'GH',
              'GURM_revisado',
              'CLMN10SUE',
              'RLMN',
              'RRMNS_2',
              'JGMN6SUE',
              'FGH_EEGdescompuesto',
              'MGNA',
              'EMNN')
nomb_arch = c('VCNNS1',
              'MJNNVIGILOS',
              'JANASUE',
              'GH24031950SUEÑO',
              'GURM251148SUE',
              'CLMN10SUE',
              'RLMN10SUE',
              'RRMNS',
              'JGMN6SUE',
              'FGHSUE',
              'MGNA5SUE',
              'EMNNS')
nomb_facil = c('VCR',
               'MJH',
               'JAE',
               'GHA',
               'MFGR',
               'CLO',
               'RLO',
               'RRU',
               'JGZ', 
               'FGH',
               'MGG',
               'EMT')

frecuenciasss = c(200,
                  512,512,
                  200,200,
                  512,512,
                  200,
                  512,512,
                  512,512)

if(frecuencia==512){
  if(duracion==60){
    # 60 s  : 2 epocas por bloque
    indixe = ceiling(indice/2)
    
    indixe = unique(indixe)
    indixe = sort(indixe)
    
    indice = indixe
    
    epo_s_min = 1
  }
  if(duracion==30){
    # 30 s  : 1 epoca por bloque
    # no se hace nada
    
    epo_s_min = 2
  }
  if(duracion==10){
    # 10 s  : 1 epoca 3 bloques
    indixe = c( 3*indice  ,
                3*indice-1,
                3*indice-2)
    
    indixe = unique(indixe)
    indixe = sort(indixe)
    
    indice = indixe
    
    epo_s_min = 6
  }
  if(duracion==2.5){
    # 2.5 s : 1 epoca 12 bloques
    indixe = c( 12*indice  ,
                12*indice- 1,
                12*indice- 2,
                12*indice- 3,
                12*indice- 4,
                12*indice- 5,
                12*indice- 6,
                12*indice- 7,
                12*indice- 8,
                12*indice- 9,
                12*indice-10,
                12*indice-11)
    
    indixe = unique(indixe)
    indixe = sort(indixe)
    
    indice = indixe
    
    epo_s_min = 24
  }
}
if(frecuencia==200){
  if(duracion==60){
    # 60 s  : 6 epocas en 1 bloque
    indixe = ceiling(indice/6)
    
    indixe = unique(indixe)
    indixe = sort(indixe)
    
    indice = indixe
    
    epo_s_min = 1
  }
  if(duracion==30){
    # 30 s  : 3 epocas en 1 bloque
    indixe = ceiling(indice/3)
    
    indixe = unique(indixe)
    indixe = sort(indixe)
    
    indice = indixe
    
    epo_s_min = 2
  }
  if(duracion==10){
    # 10 s  : 1 epoca por bloque
    # no se hace nada
    
    epo_s_min = 6
  }
  if(duracion==2.5){
    # 2.5 s : 1 epoca 4 bloques
    indixe = c( 4*indice  ,
                4*indice-1,
                4*indice-2,
                4*indice-3)
    
    indixe = unique(indixe)
    indixe = sort(indixe)
    
    indice = indixe
    
    epo_s_min = 24
  }
}

rect(x0,y0,x1,y1, col= rgb(0,0,1.0,alpha=0.5))