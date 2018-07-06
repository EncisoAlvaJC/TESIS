# 
# Esa funcion grafica segmentos de EEG, para ilustrar

#
# para vc, EMG es una copia de LOG
#

# directorios de trabajo
data_dir    = 'C:/Users/Erika/Desktop/Julio161213/ORIGINAL/'
central_dir = 'C:/Users/Erika/Desktop/Julio161213/scripts170506'
result_dir  = 'C:/Users/Erika/Desktop/Julio161213/scripts170506/test/'

channel   = c('C3','C4','CZ',
              'F3','F4','F7','F8',
              'FP1','FP2','FZ',
              'O1','O2','P3','P4','PZ',
              'T3','T4','T5','T6',
              'LOG','ROG',
              'EMG'
)

kolor     = c('black','black','black',
              'blue','blue','blue','blue',
              'blueviolet','blueviolet','blueviolet',
              'black','black','red','red','red',
              'black','black','black','black',
              'red','red',
              'black')

nom_dir  = c('CLMN10SUE',
             'JANASUE',
             'JGMN6SUE',
             'MJNNVIGILOScCanal',
             'RLMN',
             'RRMNS_2',
             'VCNNS',
             'FGH_EEGdescompuesto',
             'GURM',
             'EMNN',
             'MGNA')
nom_arch = c('CLMN10SUE',
             'JANASUE',
             'JGMN6SUE',
             'MJNNVIGILOS',
             'RLMN10SUE',
             'RRMNS',
             'VCNNS1',
             'FGHSUE',
             'GH24031950SUEÑO',
             'EMNNS',
             'MGNA5SUE')

nomb_facil = c('CLO',
               'JAE',
               'JGZ', 
               'MJH',
               'RLO',
               'RRU',
               'VCR',
               'FGH',
               'MFGR',
               'EMT',
               'MGG',
               'GHA')

frecuenciasss = c(512,512,512,
                  512,512,
                  200,200,
                  512,
                  200,
                  512,512)
dur_epooo     = c(30,30,30,
                  30,30,
                  10,10,
                  30,
                  10,
                  30,30)


# variables propias del sujeto
sujeto   = 4#3
epoca_no = 190#367
#grabar   = F

frec     = frecuenciasss[sujeto]
dur_epo  = dur_epooo[sujeto]
nombre   = nom_arch[sujeto]
w_dir = paste0(data_dir,nom_dir[sujeto])
#res_dir  = paste0(result_dir,nom_dir[sujeto])

dur_epoca = frec*dur_epo

i   = epoca_no
Q   = length((i*dur_epoca) : ((i+1)*dur_epoca))
MAT = matrix(nrow=22,ncol=Q)

row.names(MAT) = channel
colnames(MAT)  = 1:Q

for(ch in 1:22){
  print(ch)
  # componentes de los nombres de archivo
  canal  = channel[ch]
  nom_ar = paste0(nombre,'_',canal,'.txt')
  
  # cargar datos
  setwd(w_dir)
  DATA = scan(nom_ar)
  DATA = as.numeric(unlist(DATA))
    
  # cuantas epocas contiene el archivo/segmento en cuestion
  max_epoca = floor(length(DATA)/dur_epoca)
  
  i = epoca_no
  
  if( i<(max_epoca+1) ){
    temp     = DATA[ (i*dur_epoca) : ((i+1)*dur_epoca) ]
    MAT[ch,] = DATA[ (i*dur_epoca) : ((i+1)*dur_epoca) ]
  }else{
    print('ERROR : # de epoca mayor al maximo')
  }
}

setwd(central_dir)

# write.csv(MAT,file=paste0('matriz_',
#                           nomb_facil[sujeto],
#                           '_',
#                           toString(epoca_no),
#                           '.csv'))


write.table(MAT,file=paste0('matriz_',
                            nomb_facil[sujeto],
                            '_',
                            toString(epoca_no),
                            '.csv'),
            col.names=F,row.names=F,sep=',')
