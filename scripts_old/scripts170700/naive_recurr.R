grabar = T

if(sujeto == 1){
  nom_dir  = 'CLMN10SUE'
  nom_arch = 'CLMN10SUE'
  etiqueta = 'CLMN'
  epoca    = 166
}else{
  nom_dir  = 'MJNNVIGILOScCanal'
  nom_arch = 'MJNNVIGILOS'
  etiqueta = 'MJNN'
  epoca    = 183
}
  
w_dir = getwd()
r_dir = paste0(getwd(),'/res_recurr')

channel = c('C3','C4','CZ','EMG','F3','F4','F7','F8',
            'FP1','FP2','FZ','LOG','O1','O2','P3','P4','PZ',
            'ROG','T3','T4','T5','T6')


mini = 1
#maxi = 1000


#ch = 8

canal  = channel[ch]
nom_ar = paste0(nom_arch,'_',canal,'_',epoca,'.txt')

# cargar datos
setwd(w_dir)
DATA  = read.csv(nom_ar)
DATA  = as.numeric(unlist(DATA))

DATA = DATA[mini:maxi]

# is.visible = function(y,a,b){
#   isit = TRUE
#   c    = a+1
#   while( (isit) && (c<b) ){
#     isit = y[c] < ( y[b]+(y[a]-y[b])*(b-c)/(b-a) )
#     c    = c+1
#   }
#   return(isit)
# }

if(grabar){
  setwd(r_dir)
  png(paste0(etiqueta,'_',canal,'_',toString(epoca),
             '_max',toString(maxi),
             '_recurr.png'),
      units='in',res=300,width=12,height=6)
}

recurr(DATA,m=2,d=2)

if(grabar){
  dev.off()
}

setwd(w_dir)