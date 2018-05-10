###########################################################
## Ejemplos de escalamiento  ##############################

#dir_datos = 'C:/Users/EQUIPO 1/Desktop/julio/tesis_respaldo/ejemplos_viejos'
dir_graf  = 'C:/Users/EQUIPO 1/Desktop/julio/tesis_respaldo/TESIS/scripts_graf_res'

dir_info       = 'C:/Users/EQUIPO 1/Desktop/julio/tesis_respaldo/TESIS/articulo_dfa'
dir_scripts    = 'C:/Users/EQUIPO 1/Desktop/julio/tesis_respaldo/TESIS/articulo_dfa/scripts'
dir_registro   = 'C:/Users/EQUIPO 1/Desktop/julio/DATOS_corregido/'

require('ggpubr')
require('locits')
require('fractal')
require('hms')
require('scales')

require('reshape2')

require('beepr')
require('readxl')

# sub-rutinas que acortan el codigo
source(paste0(dir_scripts,'/utileria.R'))

###############################################################################
# datos generales
orden_stam = T

info     = read_excel(paste0(dir_info,'/info_tecnico.xlsx'))

kanales  = read_excel(paste0(dir_info,'/info_canales.xlsx'),
                      sheet='Alfabetico')
if(orden_stam){
  kanales  = read_excel(paste0(dir_info,'/info_canales.xlsx'),
                        sheet='Stam')
}
n.canales = length(kanales$Etiqueta)

nom_dir   = info$Nombre_directorio
nom_arch  = info$Nombre_archivo
nom_facil = info$Nombre

frecuenciasss = info$Fr_muestreo
grupo_de      = info$Grupo_n

h_ini = info$hh_0
m_ini = info$mm_0
s_ini = info$ss_0

h_fin = info$hh_f
m_fin = info$mm_f
s_fin = info$ss_f

beep()

#################################################
# cargar los datos
sujeto = 2

nombre   = nom_arch[sujeto]
etiqueta = nom_facil[sujeto]

dir_datos = paste0(dir_registro,nom_dir[sujeto])
#dir_res   = dir_resultados

fr_muestreo = frecuenciasss[sujeto]


setwd(dir_datos)

canales = kanales$Nombre_archivo
ch = 1

# construye el nombre del archivo
ch_actual   = canales[ch]
nom_archivo = paste0(nombre,'_',ch_actual,'.txt')

DATOS = read.csv(nom_archivo)
DATOS = as.numeric(unlist(DATOS))

d.epo = 512*30

X.t = DATOS[d.epo*185+seq(0,d.epo)]

tmp = (0:d.epo)/512

m = rbind(X.t,tmp)

m = as.data.frame(t(m))
colnames(m) = c('proceso','tiempo')

MACH1 = ggplot(m,aes(x=tiempo,y=proceso)) +
  xlab('Tiempo [s]')+ylab('Amplitud [mV]')+
  labs(title='Canal : Fp2 | Época : 185 (MOR)')+
  theme_bw() +
  theme(panel.background = element_rect(fill = 'transparent',colour = NA),
        plot.background = element_rect(fill = 'transparent',colour = NA))+
  scale_x_continuous(expand=c(0,0),
                     breaks = pretty_breaks(n=8))+
  #theme(axis.text.x = element_blank())+
  #annotate('rect',xmin=   0,xmax=1000,ymin=-Inf,ymax=Inf,
  #         color='gray80',alpha=.2) +
  #annotate('rect',xmin=2000,xmax=3000,ymin=-Inf,ymax=Inf,
  #         color='gray80',alpha=.2) +
  #annotate('rect',xmin=4000,xmax=5000,ymin=-Inf,ymax=Inf,
  #         color='gray80',alpha=.2) +
  #annotate('rect',xmin=6000,xmax=7000,ymin=-Inf,ymax=Inf,
  #         color='gray80',alpha=.2) +
  rotate_x_text(angle = 45) +
  geom_line()
plot(MACH1)

ggsave('psg.pdf',device='pdf',dpi=600,
       width = 15,height = 8,unit='cm', 
       path=dir_graf)


#################################################################
#################################################################

uu = (1:513 - 257)/256

gg = 3/((pi*(uu))**2)*( (sin(pi*uu)/(pi*uu)) - cos(pi*uu) )
gg[is.na(gg)]=1

ig = sum(gg**2)

gg = gg/ig

UU = matrix(NA,ncol=length(X.t)+10,nrow=256)

for(t0 in 1:(length(X.t)-512) -1 +256 ){
  print(100*(t0+2-256)/(length(X.t)-512))
  
  tmp = X.t[t0 + 1:513]*gg
  
  fou = abs(fft(tmp))**2
  fou = fou[1:256]
  
  UU[,t0+256] = fou
}

UU = as.data.frame(t(UU))
colnames(UU) = 1:256
UU$tiempo = 1:length(UU[,1])-1

require('reshape2')

UU2 = melt(UU,id.vars = 'tiempo')
colnames(UU2) = c('tiempo','frecuencia','U')

UU2$frecuencia = as.numeric(as.character(UU2$frecuencia))

Uplot = ggplot(UU2,aes(x=tiempo,y=frecuencia,fill=log(U))) +
  theme_bw()+
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_distiller(palette = 'Spectral')+
  theme(legend.position = 'bottom') +
  xlab('t') + ylab(expression(omega)) +
  labs(fill=expression(paste('log( |',
                             U(t,omega),'|',phantom(.)^2,
                             ')' )))+ 
  scale_x_continuous(expand=c(0,0),
                     breaks = pretty_breaks(n=8))+
  rotate_x_text(angle = 45) +
  geom_raster()
plot(Uplot)


ggsave('psg_logU.png',device='png',dpi=600,
       width = 15,height = 8,unit='cm', 
       path=dir_graf)

#################################################################
#################################################################

uu = (1:513 - 257)/256

ww = 3/((pi*(uu))**2)*( (sin(pi*uu)/(pi*uu)) - cos(pi*uu) )
ww[is.na(gg)]=1

hh = matrix(NA,ncol=length(X.t)+10,nrow=256)

UU[is.na(UU)] = 0

for(t0 in 1:(length(X.t)-512-256) -2+512 ){
  print(100*(t0+2-256)/(length(X.t)-512))
  for(om in  1:(length(UU[1,])-1)){
    #print(t0)
    #print(om)
    #print(NULL)
    
    tmp = sum(UU[t0 + 1:513,om]*gg)
    
    #hh[,t0+(1:length(tmp))-1] = tmp
    hh[om,t0] = tmp
  }
}

hh = as.data.frame(t(hh))
colnames(hh) = 1:256
hh$tiempo = 1:length(hh[,1])-1

require('reshape2')

hh2 = melt(hh,id.vars = 'tiempo')
colnames(hh2) = c('tiempo','frecuencia','U')

hh2$frecuencia = as.numeric(as.character(hh2$frecuencia))

hplot = ggplot(hh2,aes(x=tiempo,y=frecuencia,fill=log(U))) +
  theme_bw()+
  rotate_x_text(angle = 45) +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_distiller(palette = 'Spectral')+
  theme(legend.position = 'bottom') +
  xlab('t') + ylab(expression(omega)) +
  labs(fill=expression(paste('log( ',h(t,omega),' )' )))+ 
  scale_x_continuous(expand=c(0,0),
                     breaks = pretty_breaks(n=8))+
  rotate_x_text(angle = 45) +
  geom_raster()

plot(hplot)

ggsave('psg_logh.png',device='png',dpi=600,
       width = 15,height = 8,unit='cm', 
       path=dir_graf)

##################################################################
##################################################################

#z   = stationarity(X.t,n.block=14)
z   = stationarity(X.t)
print(z)

w = as.data.frame(z$anova)
w = w[1:(length(w[,1])-1),1:(length(w[1,])-1)]

w$bloque = 1:(length(w[,1]))

colnames(w) = c(z$freq,'bloque')

w2 = melt(w,id.vars='bloque')
colnames(w2) = c('bloque','frec','Y')

w2$frec = as.numeric(as.character(w2$frec))*(1181/2)

A = ggplot(w2,aes(x=bloque,y=frec,fill=Y-5)) +
  theme_bw()+
  rotate_x_text(angle = 45) +
  scale_x_continuous(expand = c(0,0),breaks = 1:14) +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_distiller(palette = 'Spectral')+
  theme(legend.position = 'bottom') +
  xlab('Número de bloque') + ylab(expression(omega)) +
  labs(fill=expression(Y(t,omega)))+ 
  geom_raster()
plot(A)

ggarrange(MACH1,Uplot,hplot,A,ncol=1,nrow=4,align = 'v',
          heights = c(.19,.27,.27,.27),labels='AUTO')

ggsave('psg_varios.png',device='png',dpi=600,
       width = 15,height = 17,unit='cm', 
       path=dir_graf,scale=1.4)



ggarrange(MACH1,hplot,A,ncol=1,nrow=3,align = 'v',
          heights = c(.3,.4,.4),labels='AUTO')

ggsave('psg_varios_v2.png',device='png',dpi=600,
       width = 15,height = 17,unit='cm', 
       path=dir_graf,scale=1)

##################################################################
##################################################################

aa = hh[c( 512*c(2.5,7.5,12.5,17.5,22.5,27.5)),]

aa$tiempo = factor(aa$tiempo,
                   labels=c('Bloque 1','Bloque 2','Bloque 3','Bloque 4',
                            'Bloque 5','Bloque 6'))

bb = melt(aa,id.vars = 'tiempo')
colnames(bb) = c('tiempo','frec','hh')

bb$frec = as.numeric(as.character(bb$frec))
bb2 = bb[is.element(bb$frec,c(1,50,100,150,200,250)),]

cc = dcast(bb2,tiempo~frec)
cc[,2:7] = log(cc[,2:7])

MM = ggplot(bb,aes(x=frec,y=log(hh))) +
  xlab(expression(omega))+
  ylab(expression(Y(t[i],omega)))+
  theme_bw() +
  facet_grid(tiempo~.) +
  theme(panel.background = element_rect(fill = 'transparent',colour = NA),
        plot.background = element_rect(fill = 'transparent',colour = NA))+
  #scale_x_continuous(expand=c(0,0),
  #                   breaks = pretty_breaks(n=8))+
  #theme(axis.text.x = element_blank())+
  expand_limits(x=c(0,250))+
  rotate_x_text(angle = 45) +
  geom_line(color='gray') +
  geom_point(data=bb2,aes(x=frec,y=log(hh)))
plot(MM)

ggsave('psg_Y_fragmentado.pdf',device='pdf',dpi=600,
       width = 15,height = 16,unit='cm', 
       path=dir_graf)

#ggarrange(NN,MM,ncol=2,nrow=1,align='h',labels='AUTO')

#ggsave('proceso_Z_fragmentado_doble.pdf',device='pdf',dpi=600,
#       width = 15,height = 16,unit='cm', 
#       path=dir_graf)
