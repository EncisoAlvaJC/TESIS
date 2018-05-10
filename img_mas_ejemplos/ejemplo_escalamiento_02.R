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
# proceso MA cambiante, anchi de banda finito

phi = function(t){
  return((tanh(t)+1)/2)
}

set.seed(2018)

mini = -4
maxi =  4

tmp  = seq(mini,maxi,by=1/(512*8))
lon  = length(tmp)

X.t = rep(0,lon)

lon2 = ceiling(phi(mini)*(512*8)) + ceiling(phi(maxi)*(512*8)) + 1
t.0  = ceiling(phi(mini)*(512*8))

eps = rnorm( lon + lon2 )

for( i in 1:lon ){
  t = tmp[i]
  
  t.ini = t.0 + i - ceiling(phi(t)*(512*8))
  t.fin = t.0 + i + ceiling(phi(t)*(512*8))
  
  X.t[i] = sum(eps[t.ini:t.fin])#/(t.fin-t.ini)
}

for( i in 1:lon ){
  t = tmp[i]
  
  X.t[i] = X.t[i]/sqrt(floor(2*phi(t)*512*8)/(512*8))
}

m = rbind(X.t,tmp)

m = as.data.frame(t(m))
colnames(m) = c('proceso','tiempo')

MACH1 = ggplot(m,aes(x=tiempo,y=proceso)) +
  xlab(NULL)+ylab(NULL)+
  theme_bw() +
  theme(panel.background = element_rect(fill = 'transparent',colour = NA),
        plot.background = element_rect(fill = 'transparent',colour = NA))+
  scale_x_continuous(expand=c(0,0))+
  #theme(axis.text.x = element_blank())+
  geom_line()

plot(MACH1)

###########################################################
###########################################################
###########################################################

#stop()

X = m$proceso

mm = c()
ee = c()
nn = rep(1:(2**(15-10)),15-10+1)
#nn = c()

for(expon in 10:15){
  f.extra = 2**(15-expon)
  dep = 2**expon
  cmp = dep/(2**10)
  
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

rr$Expon = 2**rr$Expon/(512*16)

A = ggplot(rr,aes(x=Tiempo,y=Expon,fill=Est))+
  theme_bw()+
  xlab('Tiempo [s]') + ylab('Tamaño de ventana [s]') +
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
  xlab(NULL) + ylab('Amplitud')+
  theme_bw()+
  geom_line()
plot(B)

C = ggarrange(B,A,ncol=1,nrow=2,align='v',heights = c(.4,.6))

plot(C)

ggsave('stat_local_artificial.pdf',device='pdf',dpi=600,
       width = 15,height = 10,unit='cm',scale=1.1)

stop()

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



