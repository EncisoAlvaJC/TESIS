# 
# Esa funcion grafica segmentos de EEG, para ilustrar

#
# para vc, EMG es una copia de LOG
#

# directorios de trabajo
data_dir    = 'C:/Users/Erika/Desktop/Julio161213/ORIGINAL/'
central_dir = 'C:/Users/Erika/Desktop/Julio161213/scripts170405'
result_dir  = 'C:/Users/Erika/Desktop/Julio161213/scripts170405/p_170427/'

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

grupo_de = c(1,2,1,0,1,1,0,-1,0,0,2)

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
sujeto   = 3#4
epoca_no = 367#190
SKALA    = 10
#grabar   = F

frec     = frecuenciasss[sujeto]
dur_epo  = dur_epooo[sujeto]
nombre   = nom_arch[sujeto]
w_dir = paste0(data_dir,nom_dir[sujeto])
#res_dir  = paste0(result_dir,nom_dir[sujeto])

dur_epoca = frec*dur_epo

i = epoca_no
Q = length((i*dur_epoca) : ((i+1)*dur_epoca))

MAT = matrix(nrow=22,ncol=Q)
  
#if(grabar){
#  setwd(result_dir)
#  #png(paste0(nomb_facil[sujeto],'_',
#  pdf(paste0(nomb_facil[sujeto],'_',
#             toString(epoca_no),'_',
#             'PDG',
#             '_lucirse_PSG',
#             '.pdf'),width=12,height=6)
#  #'.png'),units='cm',res=150,width=20,height=11)
#}

#ch = 1
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
  
  # if(ch == 1){
  #   par(bg = 'NA', mar = c(3,3,2,1.3))
  #   
  #   ex = ((i*dur_epoca) : ((i+1)*dur_epoca))/(frec)
  #   
  #   ex_h = floor(ex/(60*60))
  #   ex_m = floor((ex-ex_h*60*60)/60)
  #   ex_s = floor((ex-ex_h*60*60-ex_m*60))
  #   
  #   ex_etiqueta = c()
  #   for(i in 1:length(ex_h)){
  #     if(ex_m[i]<10){
  #       cero_m = '0'
  #     }else{
  #       cero_m = ''
  #     }
  #     if(ex_s[i]<10){
  #       cero_s = '0'
  #     }else{
  #       cero_s = ''
  #     }
  #     etiq = paste0(toString(ex_h[i]),':',
  #                   cero_m,
  #                   toString(ex_m[i]),':',
  #                   cero_s,
  #                   toString(ex_s[i]))
  #     ex_etiqueta = c(ex_etiqueta,etiq)
  #   }
  #   
  #   skip  = seq(1,length(ex),frec*5)
  #   skip2 = seq(1,length(ex),frec*1)
  #   
  #   plot(#ex_s+60*ex_m,
  #        temp*0,type='l',col='white',
  #        ylim=c(0,22*SKALA),
  #        xlim=c(frec,frec*29),
  #        xlab = 'Tiempo (hh:mm:ss)',
  #        ylab = '',
  #        yaxt='n',xaxt='n',
  #        main=paste0('Sujeto : ',nomb_facil[sujeto]),
  #        mgp=c(2,1,0)
  #      )
  #   #for(i in skip2){
  #   #  abline(v=skip2,col='gray')
  #   #}
  #   #axis(2,at=(22-(1:22))*SKALA+SKALA/2,labels=channel,las=1)
  #   #axis(1,at=skip,labels=ex_etiqueta[skip])
  # }
  #lines(temp+(22-ch)*SKALA+SKALA/2,
  #      type='l',col=kolor[ch])
}

if(grabar){
  dev.off()
}

write.csv(MAT,file=paste0('matriz_',
                          nomb_facil[sujeto],
                          '_',
                          toString(epoca_no),
                          '.csv'))
# ,
#           row.names=F,col.names=F)
