#####################################################################
#  GENERAR UN EJEMPLO

# library(tuneR)
# 
# set.seed(2017)
# 
# ej_fr = 512*2
# ej_dr = 2
# ej_X  = noise(kind=c('pink'),samp.rate=ej_fr,duration=ej_dr,
#                    xunit=c('time'))
# ej_XX = attr(ej_X,'left')
# ej_tt = seq(1:(ej_fr*ej_dr))/ej_fr
# 
# ej_tmp = ej_XX
# 
# ej_X  = noise(kind=c('red'),samp.rate=ej_fr,duration=ej_dr,
#               xunit=c('time'))
# ej_XX = attr(ej_X,'left')
# ej_tt = seq(1:(ej_fr*ej_dr))/ej_fr
# 
# ej_XX = c(ej_tmp,ej_XX)
# ej_tmp = ej_XX
# 
# ej_X  = noise(kind=c('white'),samp.rate=ej_fr,duration=ej_dr,
#               xunit=c('time'))
# ej_XX = attr(ej_X,'left')
# ej_tt = seq(1:(ej_fr*ej_dr))/ej_fr
# 
# ej_XX = c(ej_tmp,ej_XX)
# 
# ej_tt = seq(1:(ej_fr*ej_dr*3))/ej_fr
# 
# plot(ej_tt,ej_XX,type='l',
#      main='Ejemplo: Ruidos rojo, azul, blanco',
#      xlab='Tiempo (s)',ylab='mV')

setwd("C:/Users/Erika/Desktop/Julio161213/scripts170620")


sujeto = 2
mini = 1
maxi = 512*30


ch = 8

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
r_dir = paste0(getwd(),'/res')

channel = c('C3','C4','CZ','EMG','F3','F4','F7','F8',
            'FP1','FP2','FZ','LOG','O1','O2','P3','P4','PZ',
            'ROG','T3','T4','T5','T6')

canal  = channel[ch]
nom_ar = paste0(nom_arch,'_',canal,'_',epoca,'.txt')

# cargar datos
setwd(w_dir)
DATA  = read.csv(nom_ar)
DATA  = as.numeric(unlist(DATA))

ss = seq(mini,maxi,by=4)

DATA = DATA[ss]

plot((ss)/512,DATA,type='l',
           main=paste0(etiqueta,' , ',canal),
           xlab='Tiempo (s)',ylab='mV')

#####################################################################
#  FUNCIONES VENTANA

# Funciion de respuesta a frecuencia
# Tukey-Hanning
gg = function(uu,MM){
  val = 0
  if(abs(uu)<MM){
    val = 0.5 + 0.5*cos(pi*uu/MM)#/(pi*uu/MM)
  }
  return(val)
}

# Ventana de retrasos
# Tukey-Hanning
ll = function(uu,MM){
  val = 0
  if(abs(uu)<MM){
    val = 0.5 + 0.5*cos(pi*uu/MM)#/(pi*uu/MM)
  }
  return(val)
}

#####################################################################
#  ESTIMADOR DE DOBLE VENTANA

# librerias
library(plotrix)
library(rgl)

# parametros
n_om  = 100   # numero de frecuencias
debug = T

# datos ejemplo
#Xt = ej_XX
Xt = DATA

# datos dados
TT = length(Xt)

# variables de apoyo
OM = seq(0,pi,pi/n_om)
OO = length(OM)
ii = complex(real=0,imaginary=1)

UU = matrix(nrow=TT,ncol=OO)

MM = 512/8

for(ww in 1:OO){
  wi = OM[ww]
  for(ti in 1:TT){
    U_tmp = 0
    
    subb = max(1 ,ti-MM/2)
    supp = min(TT,ti+MM/2)
    for(tj in subb:supp){
      U_tmp = U_tmp + gg(ti-tj,MM)*Xt[tj]*exp(ii*wi*tj)
    }
    UU[ti,ww] = U_tmp 
  }
  print(toString(floor(ww/OO*100*10)/10))
}

UU = Mod(UU)**2

#color2D.matplot(t(UU),border=NA,
#                main='Estimador U**2')

#color2D.matplot(log(t(UU)),border=NA,
#                main='Estimador log(U**2)')

#jet.colors <-   # function from grDevices package
#  colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
#                     "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
#colorzjet <- jet.colors(100)  # 100 separate color 
#col.ind <- cut(log(UU),100)
#open3d()
#persp3d(x=1:TT,y=1:OO,z=log(UU),col=colorzjet[col.ind])

#heatmap(log(t(UU)),Rowv=NA,Colv=NA)

ff = matrix(nrow=TT,ncol=OO)

for(ww in 1:OO){
  wi = OM[ww]
  for(ti in 1:TT){
    f_tmp = 0
    
    subb = max(1 ,ti-MM/2)
    supp = min(TT,ti+MM/2)
    for(tj in subb:supp){
      f_tmp = f_tmp + ll(ti-tj,MM)*UU[tj,ww]
    }
    ff[ti,ww] = f_tmp 
  }
  print(toString(floor(ww/OO*100*10)/10))
}

color2D.matplot(t(ff),border=NA,
                main='Estimador f')

color2D.matplot(log(t(ff)),border=NA,
                main='Estimador Y = log(f)')


jet.colors <-   # function from grDevices package
  colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                     "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
colorzjet <- jet.colors(100)  # 100 separate color 
col.ind <- cut(log(ff),100)
open3d()
persp3d(x=1:TT,y=1:OO,z=log(ff),col=colorzjet[col.ind],
        main=paste0(etiqueta,' , ',canal))
