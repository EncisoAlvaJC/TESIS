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


####################################################################################
####################################################################################
library(animation)

saveGIF({
  ani.options(interval = 0.2, nmax = 50)

  #set.seed(2016)
    
  #x = seq(-1,1,0.01)
  #r = rnorm(length(x),0,0.5)
  #y = sin(x*2*pi)+ r
  
  #t = x
  #L = x
  
  x = (1:TT)/512
  
  color.gradient <- function(x, colors=c("red","gray"), colsteps=100) {
    return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps)) ] )
  }
  
  for(i in 1:length(TT)){
    
    #R = x*0+1
    #W = exp(-20*(x-x[i])**2)
    #mod = lm( y~x, weights=W )
  
    #dev.off()
    par(mfrow=c(2,1),mar=c(1,2,1,1)+0.1)
    
    #L[i] = mod$fitted.values[i]

    k = color.gradient(-W)
        
    W = exp(-20*(x-x[i])**2)
    plot(c(1,TT)/512,c(min(Xt),max(Xt)),xlab='',ylab='',type='l',col='white',
         bty='n',xaxt='n')
    for(j in 2:TT){
      lines(c(x[j-1],x[j]),c(Xt[j-1],Xt[j]),xlab='',ylab='',type='l',col=k[j],
           bty='n',xaxt='n')
    }
    
    #plot(x,y,xlab='',ylab='')
    #plot((1:TT)/512,Xt,xlab='',ylab='',type='p',col=gray(1-W),
    #     bty='n',xaxt='n')
    axis(1,labels=F)
    lines(x[1:i],L[1:i],type='l',col='blue')
    abline(mod,col='red')
    points(x[i],L[i],col='blue',pch=19)
    
    #plot(x,W,type='l',xlab='',ylab='',bty='n')
    #lines(x,R,type='l',col='green')
    
    plot(ff[i,],type='l',bty='n')
    
    ani.pause()
  }
}, movie.name="spec.gif", ani.width=623, ani.height=400)

####################################################################################
####################################################################################

# bisquare = function(u){
#   if(u>1){
#     reg = 0
#   }
#   else{
#     reg = (1-u**2)**2
#   }
#   return(reg)
# }
# 
# h = 6*median(abs(y-L))
# R2 = R
# 
# for(i in 1:length(W)){
#   R2[i] = bisquare(abs(y[i]-L[i])/h)
# }
# 
# dev.off()
# par(mfrow=c(2,1),mar=c(1,2,1,1)+0.1)
# 
# plot(x,y,xlab='',ylab='',type='p')
# lines(x,L,type='l',col='blue')
# 
# plot(x,R2,type = 'l',col='green')
# 
# #plot(x,y-L,xlab='',ylab='')
# #abline(0,0,col='blue')
# 
# dev.off()
# par(mfrow=c(2,1),mar=c(1,2,1,1)+0.1)
# 
# plot(x,y,xlab='',ylab='',type='p',col=grey(1-W2))
# lines(x,L,type='l',col='blue')
# 
# plot(x,W2,type = 'l',col='green')
# 
# ####################################################################################
# ####################################################################################
# 
# library(animation)
# 
# saveGIF({
#   ani.options(interval = 0.2, nmax = 50)
#   
#   set.seed(2016)
#   
#   x = seq(-1,1,0.01)
#   r = rnorm(length(x),0,0.5)
#   y = sin(x*2*pi)+ r
#   
#   t = x
#   L = x
#   
#   for(i in 1:length(x)){
#     
#     #R = x*0+1
#     W = exp(-20*(x-x[i])**2)
#     mod = lm( y~x, weights=W*R2 )
#     
#     #dev.off()
#     par(mfrow=c(2,1),mar=c(1,2,1,1)+0.1)
#     
#     L[i] = mod$fitted.values[i]
#     
#     #plot(x,y,xlab='',ylab='')
#     plot(x,y,xlab='',ylab='',type='p',col=gray(1-W*R2),
#          bty='n',xaxt='n')
#     axis(1,labels=F)
#     lines(x[1:i],L[1:i],type='l',col='blue')
#     abline(mod,col='red')
#     points(x[i],L[i],col='blue',pch=19)
#     
#     plot(x,W*R2,type='l',xlab='',ylab='',bty='n',ylim=c(0,1))
#     lines(x,R2,type='l',col='green')
#     
#     ani.pause()
#   }
# }, movie.name="stl_2.gif", ani.width=623, ani.height=400)
# 
# ####################################################################################
# ####################################################################################
# 
# # library(animation)
# # 
# # saveLatex({
# #   ani.options(interval = 0.2, nmax = 50)
# #   
# #   x = seq(-1,1,0.01)
# #   r = rnorm(length(x),0,0.5)
# #   y = sin(x*2*pi)+ r
# #   
# #   t = x
# #   L = x
# #   
# #   for(i in 1:length(x)){
# #     
# #     R = x*0+1
# #     W = exp(-20*(x-x[i])**2)
# #     mod = lm( y~x, weights=W )
# #     
# #     #dev.off()
# #     par(mfrow=c(2,1),mar=c(1,2,1,1)+0.1)
# #     
# #     L[i] = mod$fitted.values[i]
# #     
# #     plot(x,y,xlab='',ylab='')
# #     lines(x[1:i],L[1:i],type='l',col='blue')
# #     abline(mod,col='red')
# #     points(x[i],L[i],col='blue',pch=19)
# #     
# #     plot(x,W,type='l',xlab='',ylab='')
# #     lines(x,R,type='l',col='green')
# #     
# #     ani.pause()
# #   }
# # }, ani.basename='STLdemo',ani.opts='controls,loop,width=\\linewidth',
# #    documentclass=NULL,latex.filename='stl_demo_latex0.tex')
# 
# ####################################################################################
# ####################################################################################
# 
# # library(animation)
# # 
# # saveGIF({
# #   
# #   ani.options(interval = 0.2, nmax = 50)
# #   
# #   t = seq(0,pi,.01)
# #   
# #   x = cos(2*t)
# #   
# #   y = sin(2*t)
# #   
# #   idx = seq(1,length(x),10)
# #   
# #   for (i in seq_along(idx)) {
# #     
# #     plot(x,y,type='n')
# #     
# #     points(x[seq(idx[i])],
# #            
# #            y[seq(idx[i])], pch=15, col='dark green')
# #     
# #     ani.pause() }
# #   
# # }, movie.name = "circle.gif",
# # 
# # ani.width = 600, ani.height = 600) 
# # 
