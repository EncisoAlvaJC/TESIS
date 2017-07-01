#
# para vc, EMG es una copia de LOG
#

library(beepr)

data_dir    = 'C:/Users/Erika/Desktop/Julio161213/ORIGINAL/'
central_dir = 'C:/Users/Erika/Desktop/Julio161213/scripts170405'
result_dir  = 'C:/Users/Erika/Desktop/Julio161213/scripts170405/estacionariedad_30s/'

setwd(central_dir)
source('multipsr10.R' ) 

nom_dir  = c('VCNNS',
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
            )
nom_arch = c('VCNNS1',
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
nom_facil = c('VCR',
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

grupo_de = c(0,0,0,0,0,1,1,1,1,-1,-1,-1)

frecuenciasss = c(200,
                  512,512,
                  200,
                  200,#segun valeria GUR=200 Hz
                  #512, #segun la libreta GURM=512
                  512,512,
                  200,#solo tiene 3 horas
                  512,
                  512,512,
                  200)

beep()

 for(sujeto in 1:12){
   nombre   = nom_arch[sujeto]
   work_dir = paste0(data_dir,nom_dir[sujeto])
   res_dir  = paste0(result_dir,nom_dir[sujeto])
   
   setwd(work_dir)
   
   w_dir = work_dir
   ch = 1
   z=1
   
   for(z in 1:22){
     multipsr(z,nombre,work_dir,central_dir,res_dir,
              frecuenciasss[sujeto],5)
   }
   beep()
 }

beep()

beep()

beep()

beep()

beep()
