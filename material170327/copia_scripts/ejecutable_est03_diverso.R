#
# para vc, EMG es una copia de LOG
#

library(beepr)

data_dir    = 'C:/Users/Erika/Desktop/Julio161213/ORIGINAL/'
central_dir = 'C:/Users/Erika/Desktop/Julio161213/scripts170308'
result_dir  = 'C:/Users/Erika/Desktop/Julio161213/scripts170308/estacionariedad_sf_60s/'

setwd(central_dir)
source('multipsr08_sinfiltro.R' ) 

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

for(sujeto in c(1:5,8,10,11)){
#for(sujeto in 1:11){
  nombre   = nom_arch[sujeto]
  work_dir = paste0(data_dir,nom_dir[sujeto])
  res_dir  = paste0(result_dir,nom_dir[sujeto])
  
  setwd(work_dir)
  
  for(z in 1:22){
    multipsr(z,nombre,work_dir,central_dir,res_dir,
             #512,30)
             512,30)
  }
  beep()
}

for(sujeto in c(6:7,9)){
  nombre   = nom_arch[sujeto]
  work_dir = paste0(data_dir,nom_dir[sujeto])
  res_dir  = paste0(result_dir,nom_dir[sujeto])
  
  setwd(work_dir)
  
  for(z in 1:22){
    multipsr(z,nombre,work_dir,central_dir,res_dir,
             #200,10)
             200,30)
  }
  beep()
}

beep()

beep()

beep()

beep()

beep()
