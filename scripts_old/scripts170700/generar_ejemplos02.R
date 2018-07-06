
# sujeto CLMN

# constantes del sujeto
#nom_dir  = 'CLMN10SUE'
#nom_arch = 'CLMN10SUE'
nom_dir  = 'MJNNVIGILOScCanal'
nom_arch = 'MJNNVIGILOS'

w_dir = paste0('C:/Users/Erika/Desktop/Julio161213/ORIGINAL/',nom_dir)
r_dir = paste0('C:/Users/Erika/Desktop/Julio161213/scripts170620/',nom_dir)

frec     = 512
dur_epo  = 30

hh_0 = 1
mm_0 = 0
ss_0 = 0

hh_f = 2
mm_f = 0
ss_f = 0

# constantes genericas
channel = c('C3','C4','CZ','EMG','F3','F4','F7','F8',
            'FP1','FP2','FZ','LOG','O1','O2','P3','P4','PZ',
            'ROG','T3','T4','T5','T6')

# constantes dependientes del sujeto
dur_epoca = frec*dur_epo

# se calculan las epocas contempladas
seg_0 = hh_0*60*60 + mm_0*60 + ss_0 + 1
seg_f = hh_f*60*60 + mm_f*60 + ss_f + 1

epo_0 = seg_0*frec
epo_f = seg_f*frec

for(ch in 1:22){
  # componentes de los nombres de archivo
  canal  = channel[ch]
  nom_ar = paste0(nom_arch,'_',canal,'.txt')

  # cargar datos
  setwd(w_dir)
  DATA  = read.csv(nom_ar)
  DATA  = as.numeric(unlist(DATA))
  
  index = seq(epo_0,epo_f,by=1)
  mini  = DATA[index]
  
  colnames(mini) = c()
  
  # los resultados se guardan en un archivo .csv
  setwd(r_dir)
  write.table(mini,nom_ar,row.names=FALSE,col.names=FALSE)
}
