###############################################################################
# carpeta central
data_dir    = 'C:/Users/EQUIPO 1/Desktop/julio/estacionariedad_180408'
central_dir = 'C:/Users/EQUIPO 1/Desktop/julio/tesis_respaldo/TESIS/scripts_graf'
info_dir    = 'C:/Users/EQUIPO 1/Desktop/julio/tesis_respaldo/TESIS/articulo_dfa'
g_dir       = 'C:/Users/EQUIPO 1/Desktop/julio/tesis_respaldo/TESIS/scripts_graf_res'
setwd(central_dir)

#################################################
# parametros del script

dur_chunk = 30*(2**0)

#for(dur_chunk in 30*(2**c(-5:4))){

p.val = .05

grabar.gral = T

graf.indv   = F
grabar.indv = F

grabar.ast  = T
p.ast  = c(.05,.01,.005,.001)
ast    = c(' ','*','**','***','****')

guardar_archivo = F
nombre_archivo  = paste0('asteriscos_',toString(dur_chunk),'.csv')

orden_stam = T

quienes = 1:14

###############################################################################
# librerias
require('readxl')
#require('xlsx')

require('ggplot2')
require('ggpubr')
#require('colo')

require('Rmisc')
require('reshape')
require('reshape2')

require('scales')

#################################################
# constantes generales
info     = read_excel(paste0(info_dir,'/info_tecnico.xlsx'))

orden_k  = read_excel(paste0(info_dir,'/info_canales.xlsx'))
kanales  = read_excel(paste0(info_dir,'/info_canales.xlsx'),
                      sheet='Alfabetico')
if(orden_stam){
  kanales  = read_excel(paste0(info_dir,'/info_canales.xlsx'),
                        sheet='Stam')
}
n.canales    = length(kanales$Etiqueta)
canales.arch = kanales$Nombre_archivo

#n.participantes = length(info$Nombre)
n.participantes = length(quienes)

#################################################
# contenedores de datos
dif_significativas            = matrix(nrow=n.canales,
                                       ncol=n.participantes)
colnames(dif_significativas)  = info$Nombre[quienes]
row.names(dif_significativas) = kanales$Etiqueta

matriz_REM  = matrix(nrow=n.canales,ncol=n.participantes+2)
matriz_nREM = matrix(nrow=n.canales,ncol=n.participantes+2)
matriz_tot  = matrix(nrow=n.canales,ncol=n.participantes+2)

colnames(matriz_REM)   = c(info$Nombre[quienes],'Canal_var','Etapa')
colnames(matriz_nREM)  = c(info$Nombre[quienes],'Canal_var','Etapa')
colnames(matriz_tot)   = c(info$Nombre[quienes],'Canal_var','Etapa')

matriz_REM[,'Canal_var']  = 1:n.canales
matriz_nREM[,'Canal_var'] = 1:n.canales
matriz_tot[,'Canal_var']  = 1:n.canales

matriz_REM[,'Etapa']  = rep('REM',n.canales)
matriz_nREM[,'Etapa'] = rep('NREM',n.canales)
matriz_tot[,'Etapa']  = rep('Total',n.canales)

#################################################
# cargar los datos
for(sujeto in 1:n.participantes){
  setwd(central_dir)
  source('porcentajes17_old_ART.R')
}

#################################################
# diferencias significativas REM VS NREM
#if(grabar.ast){
#  setwd(g_dir)
#  write.csv(dif_significativas,file=nombre_archivo)
#}

#################################################
# comparacion nueva
matriz  = rbind(matriz_REM,matriz_nREM)
matriz  = as.data.frame(matriz)
matriz$Canal_var = kanales$Etiqueta

matriz$xx = rep(kanales$x,2)
matriz$yy = rep(kanales$y,2)

matriz2 = melt(matriz,id=c('Canal_var','Etapa','xx','yy'))
matriz2$value = as.numeric(as.character(matriz2$value))
colnames(matriz2) = c('Canal_var','Etapa','xx','yy',
                      'Participante','Prop')

matriz2$Grupo = info$Grupo_n[matriz2$Participante]
matriz2$FF = info$Fr_muestreo[matriz2$Participante]

matriz2 = matriz2[matriz2$Grupo>-1,]
#matriz2 = matriz2[matriz2$FF==512,]

matriz2$Grupo = factor(matriz2$Grupo,labels=c('CTRL','MCI'))

matriz2$Etapa = factor(matriz2$Etapa,levels=c('NREM','REM'))

matriz2$GrupoEtapa = 2*as.numeric(matriz2$Grupo) + as.numeric(matriz2$Etapa)
matriz2$GrupoEtapa = factor(matriz2$GrupoEtapa,
                            labels=c('CTRL, NREM','CTRL, REM',
                                     'MCI, NREM','MCI, REM'))


ggplot(matriz2,aes(x = Grupo,y=Prop,fill=GrupoEtapa)) +
  xlab('Grupo') +
  ylab('Épocas estacionarias [%]') +
  theme_bw() +
  facet_grid(-yy~xx) +
  theme(strip.text.x = element_blank(),strip.background = element_blank())+
  theme(strip.text.y = element_blank())+
  scale_fill_brewer(palette='Paired') +
  scale_y_continuous(labels=percent,expand=c(0,0)) +
  theme(legend.position = 'bottom') +
  #expand_limits(y=c(0,1)) +
  labs(fill='Grupo, Etapa de sueño : ') +
  #stat_compare_means(label = 'p.signif',method='wilcox.test',
  #                   hide.ns = T)+
  geom_text(aes(label=Canal_var,hjust=1.5,vjust=2),
            y=Inf,x=Inf,color='gray60')+
  geom_boxplot() +
  geom_line()

#k = 1.25
#ggsave(file=paste0('comparacion_cabeza.pdf'),device='pdf',
#       width=15,height=15,scale=k,path=g_dir,unit='cm',
#       dpi=600)

big.summary = c()

matriz2$Canal_var = factor(matriz2$Canal_var,
                           levels = kanales$Etiqueta)

matriz_promedios = summarySE(matriz2,na.rm=T,
                             groupvar = c('Grupo','Etapa','Canal_var'),
                             measurevar = 'Prop')



ggplot(matriz_promedios,aes(x=Canal_var,y=100*Prop,fill=Etapa)) +
  xlab(NULL) +
  ylab('Stationary epochs [%]') +
  labs(fill=NULL)+
  theme_classic2() +
  scale_fill_manual(values=c('#4886a5','#da785f'))+
  facet_grid(Grupo~.) +
  theme(strip.background = element_blank())+
  theme(legend.position = 'top') +
  rotate_x_text(angle = 45) +
  geom_bar(stat='identity',position=position_dodge()) +
  geom_errorbar(aes(ymin=100*Prop-100*se,
                    ymax=100*Prop+100*se),
                position = position_dodge(1),width=0.4) +
  coord_cartesian(ylim=c(0,60))+
  scale_y_continuous(expand=c(0,0)) +
  stat_compare_means(data = matriz2, inherit.aes = F,
                     mapping = aes(x=Canal_var,y=100*Prop,fill=Etapa),
                     label = 'p.signif',method='wilcox.test',
                     label.y = 55, hide.ns = T)

#ggsave(filename='stationarity_fullNight.png',
#       path=g_dir,device='png',
#       width=6,height=4,unit='in',dpi=400)



for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  print(ch.actual)
  tmp   = matriz2[grep(paste0('^',ch.actual,'$'),matriz2$Canal_var),]
  tmp.m = summarySE(tmp,measurevar='Prop',groupvars = c('Grupo','Etapa'))
  
  aov   = aov(Prop ~ (Grupo) + (Etapa) +(Grupo:Etapa),
              data=tmp)
  
  k = summary(aov)
  k = k[[1]]
  
  print(k)
  
  View(k)
  invisible(readline(prompt="Presion [enter] para continuar"))
  
  aov   = aov(Prop ~ (Grupo),
              data=tmp)
  
  k = summary(aov)
  k = k[[1]]
  
  print(k)
  
  View(k)
  invisible(readline(prompt="Presion [enter] para continuar"))
  
  aov   = aov(Prop ~ (Etapa),
              data=tmp)
  
  k = summary(aov)
  k = k[[1]]
  
  print(k)
  
  View(k)
  invisible(readline(prompt="Presion [enter] para continuar"))
  
  
  
  
  aa = dcast(tmp,Participante~Etapa,value.var = 'Prop',fun.aggregate = mean)
  
  bb = as.matrix(aa[,2:3])
  
  mlmfit = lm(bb~1)
  
  M = mauchly.test(mlmfit, X = ~1)
  
  print(M)
  invisible(readline(prompt="Presion [enter] para continuar"))
  
  k = t(c(M$statistic,M$p.value))
  
  View(k)
  invisible(readline(prompt="Presion [enter] para continuar"))
  
  qs  = tmp.m
  qs2 = unlist(t(qs))
  qs2 = as.list((qs2))
  qs2 = unlist(t(qs2))
  big.summary = rbind(big.summary,qs2)
  
  #View(k)
  #invisible(readline(prompt="Presion [enter] para continuar"))
}

View(big.summary)

#################################################
#################################################

matriz2$Neuropsi = info$Neuropsi[as.numeric(matriz2$Participante)]
matriz2$Edad     = info$Edad[as.numeric(matriz2$Participante)]
matriz2$Escol    = info$Escolaridad[as.numeric(matriz2$Participante)]

matriz2.MOR  = matriz2[matriz2$Etapa=='REM',]
matriz2.NMOR = matriz2[matriz2$Etapa=='NREM',]

###############################################################################
# Neuropsi vs estacionariedad
# MOR
correlaciones     = matrix(0,nrow=n.canales,ncol=5)
correlaciones[,1] = kanales$Etiqueta
colnames(correlaciones) = c('Canal','R_Pearson','p','t','dF')
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  est.tmp = matriz2.MOR[grep(ch.actual,matriz2.MOR$Canal_var),]
  
  a = unlist(est.tmp$Neuropsi)
  b = unlist(est.tmp$Prop)
  k = cor.test(a,b,method='pearson',na.action(na.omit))
  
  correlaciones[ch,'R_Pearson'] = k$estimate
  correlaciones[ch,'p']       = k$p.value
  correlaciones[ch,'t']       = k$statistic
  correlaciones[ch,'dF']      = k$parameter
}

ggplot(matriz2.MOR,aes(x=Neuropsi,y=Prop,
                              shape=Grupo,color=Grupo))+
  ylab('Stationary epochs [%]') +
  labs(shape=NULL,color=NULL) +
  facet_wrap(~Canal_var,ncol=6) +
  theme_classic2() +
  geom_smooth(method=lm,
              mapping=aes(x=Neuropsi,y=Prop),
              inherit.aes=F,
              se=F,color='black') +
  scale_colour_discrete(guide = FALSE) +
  rotate_x_text(angle = 45)+
  theme(legend.position=c(1,.05),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  geom_point()

# NMOR
correlaciones     = matrix(0,nrow=n.canales,ncol=5)
correlaciones[,1] = kanales$Etiqueta
colnames(correlaciones) = c('Canal','R_Pearson','p','t','dF')
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  est.tmp = matriz2.NMOR[grep(ch.actual,matriz2.NMOR$Canal_var),]
  
  a = unlist(est.tmp$Neuropsi)
  b = unlist(est.tmp$Prop)
  k = cor.test(a,b,method='pearson',na.action(na.omit))
  
  correlaciones[ch,'R_Pearson'] = k$estimate
  correlaciones[ch,'p']       = k$p.value
  correlaciones[ch,'t']       = k$statistic
  correlaciones[ch,'dF']      = k$parameter
}

ggplot(matriz2.NMOR,aes(x=Neuropsi,y=Prop,
                               shape=Grupo,color=Grupo))+
  ylab('Stationary epochs [%]') +
  labs(shape=NULL,color=NULL) +
  facet_wrap(~Canal_var,ncol=6) +
  theme_classic2() +
  geom_smooth(method=lm,
              mapping=aes(x=Neuropsi,y=Prop),
              inherit.aes=F,
              se=F,color='black') +
  scale_colour_discrete(guide = FALSE) +
  rotate_x_text(angle = 45)+
  theme(legend.position=c(1,.05),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  geom_point()

###############################################################################
# Edad vs estacionariedad
# MOR
correlaciones     = matrix(0,nrow=n.canales,ncol=5)
correlaciones[,1] = kanales$Etiqueta
colnames(correlaciones) = c('Canal','R_Pearson','p','t','dF')
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  est.tmp   = matriz2.MOR[grep(ch.actual,matriz2.MOR$Canal_var),]
  
  a = unlist(est.tmp$Edad)
  b = unlist(est.tmp$Prop)
  k = cor.test(a,b,method='pearson',na.action(na.omit))
  
  correlaciones[ch,'R_Pearson'] = k$estimate
  correlaciones[ch,'p']       = k$p.value
  correlaciones[ch,'t']       = k$statistic
  correlaciones[ch,'dF']      = k$parameter
}

ggplot(matriz2.MOR,aes(x=Edad,y=Prop,
                              shape=Grupo,color=Grupo))+
  ylab('Stationary epochs [%]') +
  labs(shape=NULL,color=NULL) +
  facet_wrap(~Canal_var,ncol=6) +
  theme_classic2() +
  geom_smooth(method=lm,
              mapping=aes(x=Edad,y=Prop),
              inherit.aes=F,
              se=F,color='black') +
  scale_colour_discrete(guide = FALSE) +
  rotate_x_text(angle = 45)+
  theme(legend.position=c(1,.05),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  geom_point()

# NMOR
correlaciones     = matrix(0,nrow=n.canales,ncol=5)
correlaciones[,1] = kanales$Etiqueta
colnames(correlaciones) = c('Canal','R_Pearson','p','t','dF')
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  est.tmp   = matriz2.NMOR[grep(ch.actual,matriz2.NMOR$Canal_var),]
  
  a = unlist(est.tmp$Edad)
  b = unlist(est.tmp$Prop)
  k = cor.test(a,b,method='pearson',na.action(na.omit))
  
  correlaciones[ch,'R_Pearson'] = k$estimate
  correlaciones[ch,'p']       = k$p.value
  correlaciones[ch,'t']       = k$statistic
  correlaciones[ch,'dF']      = k$parameter
}

ggplot(matriz2.NMOR,aes(x=Edad,y=Prop,
                               shape=Grupo,color=Grupo))+
  ylab('Stationary epochs [%]') +
  labs(shape=NULL,color=NULL) +
  facet_wrap(~Canal_var,ncol=6) +
  theme_classic2() +
  geom_smooth(method=lm,
              mapping=aes(x=Edad,y=Prop),
              inherit.aes=F,
              se=F,color='black') +
  scale_colour_discrete(guide = FALSE) +
  rotate_x_text(angle = 45)+
  theme(legend.position=c(1,.05),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  geom_point()

# Significativos
cuales = c(18,19)
signif = c()
for(ch in cuales){
  ch.actual = kanales$Etiqueta[ch]
  Hurst.tmp = Hurst.MOR.promedio[grep(ch.actual,Hurst.MOR.promedio$Canal_var),]
  
  signif = rbind(signif,Hurst.tmp)
}

A = ggplot(signif,aes(x=Edad,y=Hurst,
                      shape=Grupo))+
  ylab('Hurst Exponent') + xlab('Age') +
  labs(shape=NULL,color=NULL) +
  facet_wrap(~Canal_var,ncol=6) +
  theme_classic2() +
  geom_smooth(method=lm,
              mapping=aes(x=Edad,y=Hurst),
              inherit.aes=F,
              se=F,color='gray60') +
  scale_colour_discrete(guide = FALSE) +
  theme(strip.background = element_blank())+
  theme(legend.position='top')+
  coord_cartesian(xlim=c(59,79))+
  facet_grid(Etapa~Canal_var) +
  geom_point()

cuales = c(20,21)
signif = c()
for(ch in cuales){
  ch.actual = kanales$Etiqueta[ch]
  Hurst.tmp = Hurst.NMOR.promedio[grep(ch.actual,Hurst.NMOR.promedio$Canal_var),]
  
  signif = rbind(signif,Hurst.tmp)
}

ggplot(signif,aes(x=Edad,y=Hurst,
                  shape=Grupo,color=Grupo))+
  ylab('Hurst Exponent') + xlab('Age') +
  labs(shape=NULL,color=NULL) +
  facet_wrap(~Canal_var,ncol=6) +
  theme_classic2() +
  scale_color_manual(values=c('blue','red'),guide=F)+
  geom_smooth(method=lm,
              mapping=aes(x=Edad,y=Hurst),
              inherit.aes=F,
              se=F,color='gray60') +
  #scale_colour_discrete(guide = FALSE) +
  theme(strip.background = element_blank())+
  theme(legend.position='top')+
  coord_cartesian(xlim=c(59,79))+
  facet_grid(Etapa~Canal_var) +
  geom_point()
ggarrange(B,ncol=1,nrow=2,labels='AUTO',common.legend=TRUE)

ggsave(filename='/Fig02_edad_hurst.png',path=dir_graf,
       device='png',units='cm',width=7,height=4,dpi=400,scale=2)

ggarrange(A,labels=NULL)
ggsave(filename='/Fig_age_hurst.png',path=dir_graf,
       device='png',units='cm',width=4,height=4,dpi=400,scale=2)

###############################################################################
# Escol vs estacionariedad
# MOR
correlaciones     = matrix(0,nrow=n.canales,ncol=5)
correlaciones[,1] = kanales$Etiqueta
colnames(correlaciones) = c('Canal','R_Pearson','p','t','dF')
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  est.tmp   = matriz2.MOR[grep(ch.actual,matriz2.MOR$Canal_var),]
  
  a = unlist(est.tmp$Escol)
  b = unlist(est.tmp$Prop)
  k = cor.test(a,b,method='pearson',na.action(na.omit))
  
  correlaciones[ch,'R_Pearson'] = k$estimate
  correlaciones[ch,'p']       = k$p.value
  correlaciones[ch,'t']       = k$statistic
  correlaciones[ch,'dF']      = k$parameter
}

ggplot(matriz2.MOR,aes(x=Escol,y=Prop,
                              shape=Grupo,color=Grupo))+
  ylab('Stationary epochs [%]') +
  labs(shape=NULL,color=NULL) +
  facet_wrap(~Canal_var,ncol=6) +
  theme_classic2() +
  geom_smooth(method=lm,
              mapping=aes(x=Escol,y=Prop),
              inherit.aes=F,
              se=F,color='black') +
  scale_colour_discrete(guide = FALSE) +
  rotate_x_text(angle = 45)+
  theme(legend.position=c(1,.05),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  geom_point()

# NMOR
correlaciones     = matrix(0,nrow=n.canales,ncol=5)
correlaciones[,1] = kanales$Etiqueta
colnames(correlaciones) = c('Canal','R_Pearson','p','t','dF')
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  est.tmp   = matriz2.NMOR[grep(ch.actual,matriz2.NMOR$Canal_var),]
  
  a = unlist(est.tmp$Escol)
  b = unlist(est.tmp$Prop)
  k = cor.test(a,b,method='pearson',na.action(na.omit))
  
  correlaciones[ch,'R_Pearson'] = k$estimate
  correlaciones[ch,'p']       = k$p.value
  correlaciones[ch,'t']       = k$statistic
  correlaciones[ch,'dF']      = k$parameter
}

ggplot(matriz2.NMOR,aes(x=Escol,y=Prop,
                               shape=Grupo,color=Grupo))+
  ylab('Stationary epochs [%]') +
  labs(shape=NULL,color=NULL) +
  facet_wrap(~Canal_var,ncol=6) +
  theme_classic2() +
  geom_smooth(method=lm,
              mapping=aes(x=Escol,y=Prop),
              inherit.aes=F,
              se=F,color='black') +
  scale_colour_discrete(guide = FALSE) +
  rotate_x_text(angle = 45)+
  theme(legend.position=c(1,.05),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  geom_point()

# Significativos
cuales = c(21,22)
signif = c()
for(ch in cuales){
  ch.actual = kanales$Etiqueta[ch]
  Hurst.tmp = Hurst.MOR.promedio[grep(ch.actual,Hurst.MOR.promedio$Canal_var),]
  
  signif = rbind(signif,Hurst.tmp)
}

A = ggplot(signif,aes(x=Escol,y=Hurst,
                      shape=Grupo,color=Grupo))+
  ylab('Hurst Exponent') + xlab('Education') +
  labs(shape=NULL,color=NULL) +
  facet_wrap(~Canal_var,ncol=6) +
  theme_classic2() +
  geom_smooth(method=lm,
              mapping=aes(x=Escol,y=Hurst),
              inherit.aes=F,
              se=F,color='gray60') +
  scale_color_manual(values=c('blue','red'),guide=F)+
  #scale_colour_discrete(guide = FALSE) +
  theme(strip.background = element_blank())+
  theme(legend.position='top')+
  #coord_cartesian(xlim=c(59,79))+
  facet_grid(Etapa~Canal_var) +
  geom_point()

cuales = c(20,21)
signif = c()
for(ch in cuales){
  ch.actual = kanales$Etiqueta[ch]
  Hurst.tmp = Hurst.NMOR.promedio[grep(ch.actual,Hurst.NMOR.promedio$Canal_var),]
  
  signif = rbind(signif,Hurst.tmp)
}

B = ggplot(signif,aes(x=Escol,y=Hurst,
                      shape=Grupo,color=Grupo))+
  ylab('Hurst Exponent') + xlab('Education') +
  labs(shape=NULL,color=NULL) +
  facet_wrap(~Canal_var,ncol=6) +
  theme_classic2() +
  scale_color_manual(values=c('blue','red'),guide=F)+
  geom_smooth(method=lm,
              mapping=aes(x=Escol,y=Hurst),
              inherit.aes=F,
              se=F,color='gray60') +
  #scale_colour_discrete(guide = FALSE) +
  theme(strip.background = element_blank())+
  theme(legend.position='top')+
  #coord_cartesian(xlim=c(59,79))+
  facet_grid(Etapa~Canal_var) +
  geom_point()
ggarrange(A,B,ncol=1,nrow=2,labels='AUTO',common.legend=TRUE)

ggsave(filename='/Fig02_Escol_hurst.png',path=dir_graf,
       device='png',units='cm',width=7,height=7,dpi=400,scale=2)

###############################################################################
# Hurst x Canal
# MOR
comparaciones     = matrix(0,nrow=n.canales,ncol=6)
comparaciones[,1] = kanales$Etiqueta
colnames(comparaciones) = c('Canal','p','dF','t','m1','m2')
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  est.tmp = matriz2.MOR[grep(ch.actual,matriz2.MOR$Canal_var),]
  
  a = est.tmp[grep('CTRL',est.tmp$Grupo),]
  a = unlist(a$Prop)
  b = est.tmp[grep('MCI',est.tmp$Grupo),]
  b = unlist(b$Prop)
  
  k = wilcox.test(a,b,na.action(na.omit))
  
  comparaciones[ch,'p']  = k$p.value
  #comparaciones[ch,'dF'] = k$parameter
  comparaciones[ch,'t']  = k$statistic
  #comparaciones[ch,'m1'] = k$estimate[1]
  #comparaciones[ch,'m2'] = k$estimate[2]
}

A = ggplot(matriz2.MOR,aes(x=Canal_var,y=Prop,fill=Grupo)) +
  theme_classic2() +
  labs(fill=NULL) +
  xlab(NULL) + ylab('Hurst Exponent') +
  geom_bar(stat='identity',position=position_dodge(),
           color='black') +
  geom_errorbar(aes(ymin=Hurst-se,ymax=Hurst+se),
                position=position_dodge(.9),width=.5) +
  coord_cartesian(ylim=c(.5,1.6))+
  scale_fill_grey(start = 1, end = 0) +
  stat_compare_means(data=Hurst.MOR.promedio,inherit.aes=F,
                     mapping=aes(x=Canal_var,y=Hurst,fill=Grupo),
                     label='p.signif',method='t.test',
                     hide.ns=T,label.y=1.6)+
  theme(legend.position='top',legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  rotate_x_text(angle = 45)
ggarrange(A,labels='AUTO')
#ggsave(filename='/comparacion_uno_mor.png',path=dir_graf,
#       device='png',units='cm',width=8,height=4,dpi=400,scale=2)

# NMOR
comparaciones     = matrix(0,nrow=n.canales,ncol=6)
comparaciones[,1] = kanales$Etiqueta
colnames(comparaciones) = c('Canal','p','dF','t','m1','m2')
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  est.tmp = matriz2.NMOR[grep(ch.actual,matriz2.NMOR$Canal_var),]
  
  a = est.tmp[grep('CTRL',est.tmp$Grupo),]
  a = unlist(a$Prop)
  b = est.tmp[grep('MCI',est.tmp$Grupo),]
  b = unlist(b$Prop)
  
  k = wilcox.test(a,b,na.action(na.omit))
  
  comparaciones[ch,'p']  = k$p.value
  #comparaciones[ch,'dF'] = k$parameter
  comparaciones[ch,'t']  = k$statistic
  #comparaciones[ch,'m1'] = k$estimate[1]
  #comparaciones[ch,'m2'] = k$estimate[2]
}

A = ggplot(matriz2.NMOR,aes(x=Canal_var,y=Prop,fill=Grupo)) +
  theme_classic2() +
  labs(fill=NULL) +
  xlab(NULL) + ylab('Hurst Exponent') +
  geom_bar(stat='identity',position=position_dodge(),
           color='black') +
  geom_errorbar(aes(ymin=Hurst-se,ymax=Hurst+se),
                position=position_dodge(.9),width=.5) +
  coord_cartesian(ylim=c(.5,1.6))+
  scale_fill_grey(start = 1, end = 0) +
  stat_compare_means(data=Hurst.NMOR.promedio,inherit.aes=F,
                     mapping=aes(x=Canal_var,y=Hurst,fill=Grupo),
                     label='p.signif',method='t.test',
                     hide.ns=T,label.y=1.6)+
  theme(legend.position='top',legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  rotate_x_text(angle = 45)
ggarrange(A,labels='B')
ggsave(filename='/comparacion_uno_nmor.png',path=dir_graf,
       device='png',units='cm',width=8,height=4,dpi=400,scale=2)

# ambos
comparaciones     = matrix(0,nrow=n.canales,ncol=6)
comparaciones[,1] = kanales$Etiqueta
colnames(comparaciones) = c('Canal','p','dF','t','m1','m2')
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  Hurst.tmp = Hurst.todo.promedio[grep(ch.actual,Hurst.NMOR.promedio$Canal_var),]
  
  a = Hurst.tmp[grep('CTL',Hurst.tmp$Grupo),]
  a = unlist(a$Hurst)
  b = Hurst.tmp[grep('MCI',Hurst.tmp$Grupo),]
  b = unlist(b$Hurst)
  
  k = t.test(a,b,na.action(na.omit))
  
  comparaciones[ch,'p']  = k$p.value
  comparaciones[ch,'dF'] = k$parameter
  comparaciones[ch,'t']  = k$statistic
  comparaciones[ch,'m1'] = k$estimate[1]
  comparaciones[ch,'m2'] = k$estimate[2]
}

###############################################################################
# Intercanales
A = ggplot(promedios.todo,aes(x=Canal_var,y=Hurst,fill=Grupo)) +
  theme_classic2() +
  labs(fill=NULL) +
  xlab(NULL) + ylab('Hurst Exponent') +
  geom_bar(stat='identity',position=position_dodge(),
           color='black') +
  geom_errorbar(aes(ymin=Hurst-se,ymax=Hurst+se),
                position=position_dodge(.9),width=.5) +
  coord_cartesian(ylim=c(.5,1.6))+
  scale_fill_grey(start = 1, end = 0) +
  stat_compare_means(data=Hurst.todo.promedio,inherit.aes=F,
                     mapping=aes(x=Canal_var,y=Hurst,fill=Grupo),
                     label='p.signif',method='t.test',
                     hide.ns=T,label.y=1.6)+
  theme(legend.position='top',legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  facet_grid(Etapa~.) +
  theme(strip.background = element_blank())+
  rotate_x_text(angle = 45)
ggarrange(A,labels='A')
ggsave(filename='/comparacion_hurst_uno_CTL_MCI.png',path=dir_graf,
       device='png',units='cm',width=8,height=6,dpi=400,scale=2)

# grafico nuevo, REM vs NREM
# usando los promedios
comparaciones.CTL     = matrix(0,nrow=n.canales,ncol=5)
comparaciones.CTL[,1] = kanales$Etiqueta
colnames(comparaciones.CTL) = c('Canal','p','dF','t','m')
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  est.tmp = matriz2[grep(ch.actual,matriz2$Canal_var),]
  est.tmp = est.tmp[grep('CTRL',est.tmp$Grupo),]
  
  a = est.tmp[grep('^REM',est.tmp$Etapa),]
  a = unlist(a$Prop)
  b = est.tmp[grep('NREM',est.tmp$Etapa),]
  b = unlist(b$Prop)
  
  k = wilcox.test(a,b,na.action(na.omit),paired = T)
  
  comparaciones.CTL[ch,'p']  = k$p.value
  #comparaciones.CTL[ch,'dF'] = k$parameter
  comparaciones.CTL[ch,'t']  = k$statistic
  #comparaciones.CTL[ch,'m'] = k$estimate[1]
}
View(comparaciones.CTL)

comparaciones.PDC     = matrix(0,nrow=n.canales,ncol=5)
comparaciones.PDC[,1] = kanales$Etiqueta
colnames(comparaciones.PDC) = c('Canal','p','dF','t','m')
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  est.tmp = matriz2[grep(ch.actual,matriz2$Canal_var),]
  est.tmp = est.tmp[grep('MCI',est.tmp$Grupo),]
  
  a = est.tmp[grep('^REM',est.tmp$Etapa),]
  a = unlist(a$Prop)
  b = est.tmp[grep('NREM',est.tmp$Etapa),]
  b = unlist(b$Prop)
  
  k = wilcox.test(a,b,na.action(na.omit),paired = T)
  
  comparaciones.PDC[ch,'p']  = k$p.value
  #comparaciones.PDC[ch,'dF'] = k$parameter
  comparaciones.PDC[ch,'t']  = k$statistic
  #comparaciones.PDC[ch,'m'] = k$estimate[1]
}
View(comparaciones.PDC)

A = ggplot(promedios.todo,aes(x=Canal_var,y=Hurst,fill=Etapa)) +
  theme_classic2() +
  labs(fill=NULL) +
  xlab(NULL) + ylab('Hurst Exponent') +
  geom_bar(stat='identity',position=position_dodge(),color='black') +
  geom_errorbar(aes(ymin=Hurst-se,ymax=Hurst+se),
                position=position_dodge(.9),width=.5) +
  coord_cartesian(ylim=c(.5,1.6))+
  scale_fill_manual(values = c('#bebada','#fb8072'))+
  stat_compare_means(data=Hurst.todo.promedio,inherit.aes=F,
                     mapping=aes(x=Canal_var,y=Hurst,fill=Etapa),
                     label='p.signif',method='t.test',
                     hide.ns=T,label.y=1.6,paired=T)+
  theme(legend.position='top',legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  facet_grid(Grupo~.) +
  theme(strip.background = element_blank())+
  rotate_x_text(angle = 45)
ggarrange(A,labels='B')
ggsave(filename='/comparacion_hurst_uno_REM_NREM.png',path=dir_graf,
       device='png',units='cm',width=8,height=6,dpi=400,scale=2)
stop()
# Significativos
cuales   = c(4,5,7,9,11,13:17)
signif   = c()
signif.m = c()
for(ch in cuales){
  ch.actual    = kanales$Etiqueta[ch]
  Hurst.tmp    = Hurst.todo.promedio[grep(ch.actual,
                                          Hurst.todo.promedio$Canal_var),]
  promedio.tmp = promedios.todo[grep(ch.actual,
                                     promedios.todo$Canal_var),]
  
  signif   = rbind(signif,Hurst.tmp)
  signif.m = rbind(signif.m,promedio.tmp)
}

A = ggplot(signif.m,aes(x=Canal_var,y=Hurst,fill=Etapa)) +
  theme_classic2() +
  labs(fill=NULL) +
  xlab(NULL) + ylab('Hurst Exponent') +
  geom_bar(stat='identity',position=position_dodge(),color='black') +
  geom_errorbar(aes(ymin=Hurst-se,ymax=Hurst+se),
                position=position_dodge(.9),width=.5) +
  coord_cartesian(ylim=c(.5,1.6))+
  scale_fill_manual(values = c('#bebada','#fb8072'))+
  stat_compare_means(data=signif,inherit.aes=F,
                     mapping=aes(x=Canal_var,y=Hurst,fill=Etapa),
                     label='p.signif',method='t.test',
                     hide.ns=T,label.y=1.6,paired=T)+
  theme(legend.position='top',legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  facet_grid(Grupo~.) +
  theme(strip.background = element_blank())+
  rotate_x_text(angle = 45)
ggarrange(A,labels='C')
ggsave(filename='/comparacion_hurst_uno_REM_NREM_signif.png',path=dir_graf,
       device='png',units='cm',width=8,height=6,dpi=400,scale=2)

stop()
