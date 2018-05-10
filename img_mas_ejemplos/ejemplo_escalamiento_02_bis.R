###########################################################
## Ejemplos de escalamiento  ##############################

#dir_datos = 'C:/Users/EQUIPO 1/Desktop/julio/tesis_respaldo/ejemplos_viejos'
dir_graf  = 'C:/Users/EQUIPO 1/Desktop/julio/tesis_respaldo/TESIS/scripts_graf_res'

require('ggpubr')
require('locits')
require('fractal')
require('hms')
require('scales')

require('reshape2')

###########################################################
# Proceso tipo Alfa

set.seed(2018)

tmp = seq(-4,4,by=1/(512*8))
lon = length(tmp)

lon2 = 100000
lon3 = 2*lon2+1

eps = rnorm( lon+ lon3 )

tm2 = ((1:lon3)-lon2-1)/((512*8))

vnt = (( sin(tm2*2*pi*11) - sin(tm2*2*pi*9) )/(tm2*2*pi))

X.t = rep(0,lon)

for(i in 1:lon){
  X.t[i] = sum(vnt*eps[1:lon3+i],na.rm = TRUE)
}

#plot(X.t,type='l')

m = rbind(X.t,tmp)

m = as.data.frame(t(m))
colnames(m) = c('proceso','tiempo')

ALFA = ggplot(m,aes(x=tiempo,y=proceso)) +
  xlab(NULL)+ylab('X(t)')+
  theme_bw() +
  theme(panel.background = element_rect(fill = 'transparent',colour = NA),
        plot.background = element_rect(fill = 'transparent',colour = NA))+
  scale_x_continuous(expand=c(0,0))+
  theme(axis.text.x = element_blank())+
  geom_line()
plot(ALFA)

m$proceso1 = m$proceso * exp(-abs(m$tiempo))

ALFA1 = ggplot(m,aes(x=tiempo,y=proceso1)) +
  xlab(NULL)+ylab(expression(Y[1](t)))+
  theme_bw() +
  theme(panel.background = element_rect(fill = 'transparent',colour = NA),
        plot.background = element_rect(fill = 'transparent',colour = NA))+
  scale_x_continuous(expand=c(0,0))+
  theme(axis.text.x = element_blank())+
  geom_line()
plot(ALFA1)

m$proceso2 = m$proceso * exp(-abs(m$tiempo)**2 / 2)

ALFA2 = ggplot(m,aes(x=tiempo,y=proceso2)) +
  xlab('t')+ylab(expression(Y[2](t)))+
  theme_bw() +
  theme(panel.background = element_rect(fill = 'transparent',colour = NA),
        plot.background = element_rect(fill = 'transparent',colour = NA))+
  scale_x_continuous(expand=c(0,0))+
  #theme(axis.text.x = element_blank())+
  geom_line()
plot(ALFA2)

ggarrange(ALFA,ALFA1,ALFA2,
          ncol=1,nrow=3,align = 'v',labels='AUTO')

#ggsave('proceso_alfa_varios.pdf',device='pdf',dpi=600,
#       width = 15,height = 16,unit='cm')

###########################################################
###########################################################
###########################################################

#stop()

X = m$proceso2

mm = c()
ee = c()
nn = rep(1:(2**(15-9)),15-9+1)
#nn = c()

for(expon in 9:15){
  f.extra = 2**(15-expon)
  dep = 2**expon
  cmp = dep/(2**9)
  
  res = rep(0,f.extra)
  ind = 1:f.extra
  for(i in 1:f.extra){
    tmp = X[(1:dep)+(i-1)*dep]
    #z   = stationarity(tmp,n.block = 4,n.taper = 8)
    z   = stationarity(tmp)
    pv  = 1*(as.numeric( attr(z,'pvals')[1])<.01 &&
               as.numeric( attr(z,'pvals')[3])<.01)
    res[i] = pv
    
    #print(as.numeric( attr(z,'pvals')[1])<.05)
    #print(as.numeric( attr(z,'pvals')[3])<.05)
    #
    #print(as.numeric( attr(z,'pvals')[1])<.05 && 
    #        as.numeric( attr(z,'pvals')[3])<.05)
    
    #z$freq
    #typeof(z$anova)
    
    w = as.data.frame(z$anova)
    w = w[1:(length(w[,1])-1),1:(length(w[1,])-1)]
    
    w$bloque = 1:(length(w[,1]))
    
    colnames(w) = c(z$freq,'bloque')
    
    w2 = melt(w,id.vars='bloque')
    colnames(w2) = c('bloque','frec','Y')
    
    w2$frec = as.numeric(as.character(w2$frec))
    
    #A = ggplot(w2,aes(x=bloque,y=frec,fill=Y)) +
    #  scale_x_continuous(expand = c(0,0)) +
    #  scale_y_continuous(expand = c(0,0)) +
    #  scale_fill_distiller(palette = 'Spectral')+
    #  theme(legend.position = 'bottom') +
    #  xlab(NULL) + ylab(NULL) +
    #  geom_raster()
    #plot(A)
    
    ser = as.data.frame(tmp)
    ser$tt = (1:length(tmp))-1
    ser$tt = ser$tt/(512*8)
    
    #B = ggplot(ser,aes(x=tt,y=tmp)) +
    #  scale_x_continuous(expand = c(0,0)) +
    #  geom_line()
    
    #C = ggarrange(B,A,ncol=1,nrow=2,align = 'v',heights = c(.4,.6))
    
    #plot(C)
    
    #print(z)
    
    #invisible(readline(prompt="Presion [enter] para continuar"))
  }
  #print(expon)
  #print(res)
  #invisible(readline(prompt="Presion [enter] para continuar"))
  
  clt = c()
  for(i in 1:length(res)){
    clt = c(clt,rep(res[i],cmp))
  }
  #mm[,expon- 9+1] = clt
  mm = c(mm,clt)
  ee = c(ee,rep(expon,length(clt)))
  
  #nn = c(nn,1:length(clt))
}

rr = as.data.frame(cbind(nn,mm,nn*0,ee))
colnames(rr) = c('Tiempo','Est','Relleno','Expon')
rr$Est = factor(rr$Est,labels=c('Estacionario','No Estacionario'))
#rr$Est = factor(rr$Est,labels=c('Estacionario'))

rr$Tiempo = (rr$Tiempo-1)
rr$Tiempo = rr$Tiempo/((max(rr$Tiempo)+1)/2)*30*4
rr$Tiempo = as.POSIXct(as.hms(rr$Tiempo))

rr$Expon = 2**rr$Expon/(512*8)

A = ggplot(rr,aes(x=Tiempo,y=Expon,fill=Est))+
  theme_bw()+
  xlab('Tiempo [s]') + ylab('Tamaño de época [s]') +
  scale_x_datetime(expand=c(0,0),labels=date_format("%M.%S"),
                   breaks = date_breaks("15 sec"))+
  scale_y_continuous(expand=c(0,0),trans = log2_trans(),
                     breaks = trans_breaks("log2", function(x) 2^x),
                     labels = trans_format("log2", math_format(2^.x))) +
  theme(legend.position='bottom') +
  theme(legend.title=element_blank()) +
  scale_fill_manual(values=c('black','White'),
                             na.value="gray")+
  rotate_x_text(angle = 45)+
  theme(legend.key = element_rect(color = 'black')) +
  geom_raster(hjust=1)
plot(A)

ex = as.data.frame(X)
colnames(ex) = c('xx')
ex$tt = 1:length(X)

B = ggplot(ex,aes(x=tt,y=xx))+
  scale_x_continuous(expand=c(0,0),breaks=NULL)+
  xlab(NULL) + ylab('Amplitud [mV]')+
  theme_bw()+
  geom_line()
plot(B)

C = ggarrange(B,A,ncol=1,nrow=2,align='v',heights = c(.4,.6))

plot(C)


###########################################################
###########################################################
###########################################################

X = m$proceso[seq(1,length(m$proceso),by=2)]

mm = c()
ee = c()
nn = rep(1:(2**(14-8)),14-8+1)
#nn = c()

for(expon in 8:14){
  f.extra = 2**(14-expon)
  dep = 2**expon
  cmp = dep/(2**8)
  
  res = rep(0,f.extra)
  ind = 1:f.extra
  for(i in 1:f.extra){
    tmp = X[(1:dep)+(i-1)*dep]
    #z   = stationarity(tmp,n.block = 4,n.taper = 8)
    z   = stationarity(tmp)
    pv  = 1*(as.numeric( attr(z,'pvals')[1])<.01 &&
               as.numeric( attr(z,'pvals')[3])<.01)
    res[i] = pv
    
    #print(as.numeric( attr(z,'pvals')[1])<.05)
    #print(as.numeric( attr(z,'pvals')[3])<.05)
    #
    #print(as.numeric( attr(z,'pvals')[1])<.05 && 
    #        as.numeric( attr(z,'pvals')[3])<.05)
    
    #z$freq
    #typeof(z$anova)
    
    w = as.data.frame(z$anova)
    w = w[1:(length(w[,1])-1),1:(length(w[1,])-1)]
    
    w$bloque = 1:(length(w[,1]))
    
    colnames(w) = c(z$freq,'bloque')
    
    w2 = melt(w,id.vars='bloque')
    colnames(w2) = c('bloque','frec','Y')
    
    w2$frec = as.numeric(as.character(w2$frec))
    
    #A = ggplot(w2,aes(x=bloque,y=frec,fill=Y)) +
    #  scale_x_continuous(expand = c(0,0)) +
    #  scale_y_continuous(expand = c(0,0)) +
    #  scale_fill_distiller(palette = 'Spectral')+
    #  theme(legend.position = 'bottom') +
    #  xlab(NULL) + ylab(NULL) +
    #  geom_raster()
    #plot(A)
    
    ser = as.data.frame(tmp)
    ser$tt = (1:length(tmp))-1
    ser$tt = ser$tt/(512*8)
    
    #B = ggplot(ser,aes(x=tt,y=tmp)) +
    #  scale_x_continuous(expand = c(0,0)) +
    #  geom_line()
    
    #C = ggarrange(B,A,ncol=1,nrow=2,align = 'v',heights = c(.4,.6))
    
    #plot(C)
    
    #print(z)
    
    #invisible(readline(prompt="Presion [enter] para continuar"))
  }
  #print(expon)
  #print(res)
  #invisible(readline(prompt="Presion [enter] para continuar"))
  
  clt = c()
  for(i in 1:length(res)){
    clt = c(clt,rep(res[i],cmp))
  }
  #mm[,expon- 9+1] = clt
  mm = c(mm,clt)
  ee = c(ee,rep(expon,length(clt)))
  
  #nn = c(nn,1:length(clt))
}

rr = as.data.frame(cbind(nn,mm,nn*0,ee))
colnames(rr) = c('Tiempo','Est','Relleno','Expon')
rr$Expon = rr$Expon + 1
rr$Est = factor(rr$Est,labels=c('Estacionario','No Estacionario'))
#rr$Est = factor(rr$Est,labels=c('Estacionario'))

rr$Tiempo = (rr$Tiempo-1)
rr$Tiempo = rr$Tiempo/((max(rr$Tiempo)+1)/2)*30*4
rr$Tiempo = as.POSIXct(as.hms(rr$Tiempo))

rr$Expon = 2**rr$Expon/(512*4)

A = ggplot(rr,aes(x=Tiempo,y=Expon,fill=Est))+
  theme_bw()+
  xlab('Tiempo [s]') + ylab('Tamaño de época [s]') +
  scale_x_datetime(expand=c(0,0),labels=date_format("%M.%S"),
                   breaks = date_breaks("15 sec"))+
  scale_y_continuous(expand=c(0,0),trans = log2_trans(),
                     breaks = trans_breaks("log2", function(x) 2^x),
                     labels = trans_format("log2", math_format(2^.x))) +
  theme(legend.position='bottom') +
  theme(legend.title=element_blank()) +
  scale_fill_manual(values=c('black','White'),
                    na.value="gray")+
  rotate_x_text(angle = 45)+
  theme(legend.key = element_rect(color = 'black')) +
  geom_raster(hjust=1)
plot(A)

ex = as.data.frame(X)
colnames(ex) = c('xx')
ex$tt = 1:length(X)

B = ggplot(ex,aes(x=tt,y=xx))+
  scale_x_continuous(expand=c(0,0),breaks=NULL)+
  xlab(NULL) + ylab('Amplitud [mV]')+
  theme_bw()+
  geom_line()
plot(B)

C = ggarrange(B,A,ncol=1,nrow=2,align='v',heights = c(.4,.6))

plot(C)

###########################################################
###########################################################
###########################################################

X = m$proceso[seq(1,length(m$proceso),by=4)]

mm = c()
ee = c()
nn = rep(1:(2**(13-8)),13-8+1)
#nn = c()

for(expon in 8:13){
  f.extra = 2**(13-expon)
  dep = 2**expon
  cmp = dep/(2**8)
  
  res = rep(0,f.extra)
  ind = 1:f.extra
  for(i in 1:f.extra){
    tmp = X[(1:dep)+(i-1)*dep]
    #z   = stationarity(tmp,n.block = 4,n.taper = 8)
    z   = stationarity(tmp)
    pv  = 1*(as.numeric( attr(z,'pvals')[1])<.01 &&
               as.numeric( attr(z,'pvals')[3])<.01)
    res[i] = pv
    
    #print(as.numeric( attr(z,'pvals')[1])<.05)
    #print(as.numeric( attr(z,'pvals')[3])<.05)
    #
    #print(as.numeric( attr(z,'pvals')[1])<.05 && 
    #        as.numeric( attr(z,'pvals')[3])<.05)
    
    #z$freq
    #typeof(z$anova)
    
    w = as.data.frame(z$anova)
    w = w[1:(length(w[,1])-1),1:(length(w[1,])-1)]
    
    w$bloque = 1:(length(w[,1]))
    
    colnames(w) = c(z$freq,'bloque')
    
    w2 = melt(w,id.vars='bloque')
    colnames(w2) = c('bloque','frec','Y')
    
    w2$frec = as.numeric(as.character(w2$frec))
    
    #A = ggplot(w2,aes(x=bloque,y=frec,fill=Y)) +
    #  scale_x_continuous(expand = c(0,0)) +
    #  scale_y_continuous(expand = c(0,0)) +
    #  scale_fill_distiller(palette = 'Spectral')+
    #  theme(legend.position = 'bottom') +
    #  xlab(NULL) + ylab(NULL) +
    #  geom_raster()
    #plot(A)
    
    ser = as.data.frame(tmp)
    ser$tt = (1:length(tmp))-1
    ser$tt = ser$tt/(512*8)
    
    #B = ggplot(ser,aes(x=tt,y=tmp)) +
    #  scale_x_continuous(expand = c(0,0)) +
    #  geom_line()
    
    #C = ggarrange(B,A,ncol=1,nrow=2,align = 'v',heights = c(.4,.6))
    
    #plot(C)
    
    #print(z)
    
    #invisible(readline(prompt="Presion [enter] para continuar"))
  }
  #print(expon)
  #print(res)
  #invisible(readline(prompt="Presion [enter] para continuar"))
  
  clt = c()
  for(i in 1:length(res)){
    clt = c(clt,rep(res[i],cmp))
  }
  #mm[,expon- 9+1] = clt
  mm = c(mm,clt)
  ee = c(ee,rep(expon,length(clt)))
  
  #nn = c(nn,1:length(clt))
}

rr = as.data.frame(cbind(nn,mm,nn*0,ee))
colnames(rr) = c('Tiempo','Est','Relleno','Expon')
rr$Expon = rr$Expon + 1
rr$Est = factor(rr$Est,labels=c('Estacionario','No Estacionario'))
#rr$Est = factor(rr$Est,labels=c('Estacionario'))

rr$Tiempo = (rr$Tiempo-1)
rr$Tiempo = rr$Tiempo/((max(rr$Tiempo)+1)/2)*30*4
rr$Tiempo = as.POSIXct(as.hms(rr$Tiempo))

rr$Expon = 2**rr$Expon/(512*4)

A = ggplot(rr,aes(x=Tiempo,y=Expon,fill=Est))+
  theme_bw()+
  xlab('Tiempo [s]') + ylab('Tamaño de época [s]') +
  scale_x_datetime(expand=c(0,0),labels=date_format("%M.%S"),
                   breaks = date_breaks("15 sec"))+
  scale_y_continuous(expand=c(0,0),trans = log2_trans(),
                     breaks = trans_breaks("log2", function(x) 2^x),
                     labels = trans_format("log2", math_format(2^.x))) +
  theme(legend.position='bottom') +
  theme(legend.title=element_blank()) +
  scale_fill_manual(values=c('black','White'),
                    na.value="gray")+
  rotate_x_text(angle = 45)+
  theme(legend.key = element_rect(color = 'black')) +
  geom_raster(hjust=1)
plot(A)

ex = as.data.frame(X)
colnames(ex) = c('xx')
ex$tt = 1:length(X)

B = ggplot(ex,aes(x=tt,y=xx))+
  scale_x_continuous(expand=c(0,0),breaks=NULL)+
  xlab(NULL) + ylab('Amplitud [mV]')+
  theme_bw()+
  geom_line()
plot(B)

C = ggarrange(B,A,ncol=1,nrow=2,align='v',heights = c(.4,.6))

plot(C)



