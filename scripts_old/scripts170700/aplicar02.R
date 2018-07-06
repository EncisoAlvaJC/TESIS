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
              'EMNN',
              'AEFP',
              'MMAS',
              'RAS',
              'LIR')
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
              'EMNNS',
              'AEFP430714SUE',
              'MMAS550429VIGREP',
              'RANAVIGREP',
              'LIVIGREPOSO')
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
               'EMT',
               'AEFP',
               'MMAS',
               'RAS',
               'LIR')

d_dir_central = 'C:/Users/Erika/Desktop/Julio161213/ORIGINAL/'
central_dir   = 'C:/Users/Erika/Desktop/Julio161213/scripts170620'
r_dir_central = 'C:/Users/Erika/Desktop/Julio161213/scripts170620/estacionariedad_'


frecuenciasss = c(200,
                  512,512,
                  200,200,
                  512,512,
                  200,
                  512,512,
                  512,512,
                  512,512,512,512)

duraciones     = c(2.5,10,30,60)
duraciones_txt = c('2_5','10','30','60')

# for(dd in 1:4){
#   for(sujeto in rev(13:16)){
#     nombre    = nomb_arch[sujeto]
#     etiqueta  = nomb_facil[sujeto]
#     dir_datos = paste0(d_dir_central,nomb_dir[sujeto])
#     dir_res   = paste0(r_dir_central,
#                        duraciones_txt[dd],'s/',
#                        nomb_dir[sujeto])
#   
#     dur_epoca = duraciones[dd]
#   
#     setwd(central_dir)
#     source('multipsr_curso01_personal.R')
#   }
# }
#rect(x0,y0,x1,y1, col= rgb(0,0,1.0,alpha=0.5))