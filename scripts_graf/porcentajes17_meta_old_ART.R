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
  ylab('�pocas estacionarias [%]') +
  theme_bw() +
  facet_grid(-yy~xx) +
  theme(strip.text.x = element_blank(),strip.background = element_blank())+
  theme(strip.text.y = element_blank())+
  scale_fill_brewer(palette='Paired') +
  scale_y_continuous(labels=percent,expand=c(0,0)) +
  theme(legend.position = 'bottom') +
  #expand_limits(y=c(0,1)) +
  labs(fill='Grupo, Etapa de sue�o : ') +
  #stat_compare_means(label = 'p.signif',method='wilcox.test',
  #                   hide.ns = T)+
  geom_text(aes(label=Canal_var,hjust=1.5,vjust=2),
            y=Inf,x=Inf,color='gray60')+
  geom_boxplot() +
  geom_line()

k = 1.25
ggsave(file=paste0('comparacion_cabeza.pdf'),device='pdf',
       width=15,height=15,scale=k,path=g_dir,unit='cm',
       dpi=600)

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

ggsave(filename='stationarity_fullNight.png',
       path=g_dir,device='png',
       width=6,height=4,unit='in',dpi=400)



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
stop()

if(FALSE){
  sujs = setdiff(colnames(matriz),c('Canal_var','Etapa'))
  
  por.REM = matriz_REM[,1:14]
  class(por.REM) = 'numeric'
  toto = matriz_tot[,1:14]
  class(toto) = 'numeric'
  
  por.REM = por.REM/toto
  
  varii = sqrt(por.REM*(1-por.REM))/toto
  varii = as.data.frame(varii)
  varii$Canal_var = kanales$Etiqueta
  varii.2 = melt(varii,id.vars = c('Canal_var'))
  
  por.REM = as.data.frame(por.REM)
  por.REM$Canal_var = kanales$Etiqueta
  por.REM$xx = kanales$x
  por.REM$yy = kanales$y
  
  por.REM.2 = melt(por.REM,id.vars = c('Canal_var','xx','yy'))
  colnames(por.REM.2) = c('Canal_var','xx','yy','Sujeto','por')
  
  por.REM.2$vari = varii.2$value
  
  por.REM.2$grupo = info$Grupo_n[por.REM.2$Sujeto]
  por.REM.2$fre = info$Fr_muestreo[por.REM.2$Sujeto]
  
  por.REM.2 = por.REM.2[por.REM.2$grupo>-1,]
  por.REM.2 = por.REM.2[por.REM.2$fre>200,]
  
  por.REM.2$grupo = factor(por.REM.2$grupo,labels=c('CTRL','MCI'))
  
  por.REM.2$Sujeto = factor(por.REM.2$Sujeto,
                            levels = c('VCR','MJH','JAE','GHA','MFGR','MGG','EMT',
                                       'CLO','RLO','RRU','JGZ','AEFP','PCM',
                                       'FGH'))
  
  ggplot(por.REM.2,aes(x=Sujeto,y=100*por,color=grupo)) +
    ylab('�pocas estacionarias [%]') + 
    xlab(NULL) +
    facet_grid(-yy~xx) +
    theme_bw()+
    scale_color_manual(values=c('#377eb8','#4daf4a'))+
    theme(strip.text.x = element_blank(),strip.background = element_blank())+
    theme(strip.text.y = element_blank())+
    rotate_x_text(angle = 75)+
    geom_text(aes(label=Canal_var,hjust=1.5,vjust=2),
              y=Inf,x=Inf,color='gray40')+
    theme(legend.position = 'bottom') +
    labs(color=NULL) +
    geom_errorbar(data=por.REM.2,
                  aes(ymin=100*por - 100*vari,ymax=100*por + 100*vari))+
    geom_point()
}


#################################################
# separacion de grupos para comparar
matriz_meh = matriz
matriz2 = melt(matriz,id.vars=c('Canal_var','Etapa'))

matriz2$value = as.numeric(as.character(matriz2$value))

colnames(matriz2) = c('Canal_var','Etapa','Participante',
                      'Proporcion')

# parche
matriz2$Grupo = info$Grupo_n[as.numeric(matriz2$Participante)]

matriz2 = matriz2[matriz2$Grupo>-1,]

matriz2$GrupoEtapa = 2*matriz2$Grupo + 1*(matriz2$Etapa=='REM')

matriz2$Canal_var  = factor(matriz2$Canal_var,
                            labels=kanales$Etiqueta)
matriz2$Grupo      = factor(matriz2$Grupo,
                            labels=c('CTRL','MCI'))
matriz2$GrupoEtapa = factor(matriz2$GrupoEtapa,
                            labels=c('CTRL NREM','CTRL REM',
                                     'MCI NREM','MCI REM'))

ggplot(matriz2,aes(x=Canal_var,y=Proporcion,fill=GrupoEtapa,
                   color=GrupoEtapa))+
  geom_boxplot() +
  xlab(NULL) + ylab('�pocas estacionarias [%]') +
  theme_classic2() +
  scale_y_continuous(labels=percent) +
  #scale_fill_brewer(palette='Paired') +
  #scale_fill_grey(start = 1, end = 0) +
  scale_fill_manual(values=c('#ffff00','#009f3c',
                             '#da785f','#4886a5'))+
  theme(legend.position=c(1,1),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  theme(legend.position = 'top')+
  labs(fill=NULL) +
  labs(title=paste('Epoch =',toString(dur_chunk),
                   's | Whole night')) +
  facet_grid(Grupo~.) +
  stat_compare_means(label = 'p.signif',method='wilcox.test',
                     hide.ns = T)+
  rotate_x_text(angle = 45)

#################################################
# parametros graficos
matriz2 = melt(matriz,id=c('Canal_var','Etapa'))

matriz2$value = as.numeric(as.character(matriz2$value))

matriz3 = matriz2
colnames(matriz3) = c('Canal_var','Etapa','Sujeto','porc')



#################################################
#################################################
stop()

colnames(matriz2) = c('Canal_var','Etapa','Participante',
                      'Proporcion','Grupo','GrupoEtapa')

matriz2$GrupoEtapa = 2*matriz2$Grupo + 1*(matriz2$Etapa=='REM')



#droplevels(matriz2$Proporcion)
#matriz2$Proporcion = droplevels(matriz2$Proporcion)

#matriz2$Grupo = 1*(matriz2$Grupo=='CTRL')
#matriz2$Etapa = 1*(matriz2$Etapa=='REM')

matriz2$Canal_var  = factor(matriz2$Canal_var,
                            labels=kanales$Etiqueta)
matriz2$Grupo      = factor(matriz2$Grupo,
                            labels=c('CTRL','PMCI'))
matriz2$GrupoEtapa = factor(matriz2$GrupoEtapa,
                            labels=c('CTRL NREM','CTRL REM',
                                     'PMCI NREM','PMCI REM'))

promedio = summarySE(matriz2,measurevar='Proporcion',
                     groupvars=c('Canal_var','GrupoEtapa'))

promedio$Grupo = 1*(promedio$GrupoEtapa=='CTRL REM')+
  1*(promedio$GrupoEtapa=='CTRL NREM')
promedio$Etapa = 1*(promedio$GrupoEtapa=='CTRL REM')+
  1*(promedio$GrupoEtapa=='PMCI REM')

promedio$Grupo = factor(promedio$Grupo,
                        labels=c('PMCI','CTRL'))
promedio$Etapa = factor(promedio$Etapa,
                        labels=c('NREM','REM'))

ggplot(promedio,aes(x=Canal_var,y=Proporcion,fill=GrupoEtapa)) +
  xlab(NULL) + ylab('Stationary epoche [%]') +
  labs(title=paste('Epoch =',toString(dur_chunk),
                   's | Whole night')) +
  theme_classic2() +
  labs(fill=NULL) +
  scale_y_continuous(labels=percent) +
  #geom_bar(stat='identity',position=position_dodge(),color='black')+
  geom_bar(stat='identity',position=position_dodge())+
  geom_errorbar(aes(ymin=Proporcion-se,ymax=Proporcion+se),
                position=position_dodge(.9),width=.5) +
  #scale_fill_grey(start = 1, end = 0) +
  scale_fill_manual(values=c('#ffff00','#009f3c',
                             '#da785f','#4886a5'))+
  #scale_y_continuous(expand=c(0,0))+
  theme(legend.position=c(1,1),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  facet_grid(Grupo~.) +
  coord_cartesian(ylim=c(0,.5))+
  stat_compare_means(data = matriz2,inherit.aes=F,
                     aes(x=Canal_var,y=Proporcion,fill=GrupoEtapa),label = 'p.signif',method='wilcox.test',
                     hide.ns = T,
                     label.y =.5)+
  rotate_x_text(angle = 45)
if(grabar.gral){
  ggsave(filename=paste0('completo_Comparacion_gpos_CTRL_PDC_',
                         toString(dur_chunk),
                         '_barra.png'),
         path=g_dir,device='png',
         width=8,height=5,unit='in',dpi=400)
  ggsave(filename=paste0('completo_Comparacion_gpos_CTRL_PDC_',
                         toString(dur_chunk),
                         '_barra.eps'),
         path=g_dir,device='eps',
         width=8,height=5,unit='in')
}

ggplot(promedio,aes(x=Canal_var,y=Proporcion,fill=GrupoEtapa)) +
  xlab(NULL) + ylab('Stationary epoche [%]') +
  labs(title=paste('Epoch =',toString(dur_chunk),
                   's | Whole night')) +
  theme_classic2() +
  labs(fill=NULL) +
  scale_y_continuous(labels=percent) +
  #geom_bar(stat='identity',position=position_dodge(),color='black')+
  geom_bar(stat='identity',position=position_dodge())+
  geom_errorbar(aes(ymin=Proporcion-se,ymax=Proporcion+se),
                position=position_dodge(.9),width=.5) +
  #scale_fill_grey(start = 1, end = 0) +
  scale_fill_manual(values=c('#ffff00','#009f3c',
                             '#da785f','#4886a5'))+
  #scale_y_continuous(expand=c(0,0))+
  theme(legend.position=c(1,1),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  facet_grid(Etapa~.) +
  coord_cartesian(ylim=c(0,.5))+
  stat_compare_means(data = matriz2,inherit.aes=F,
                     aes(x=Canal_var,y=Proporcion,fill=GrupoEtapa),label = 'p.signif',method='wilcox.test',
                     hide.ns = T,
                     label.y =.5)+
  rotate_x_text(angle = 45)
if(grabar.gral){
  ggsave(filename=paste0('completo_Comparacion_gpos_REM_NREM_',
                         toString(dur_chunk),
                         '_barra.png'),
         path=g_dir,device='png',
         width=8,height=5,unit='in',dpi=400)
  ggsave(filename=paste0('completo_Comparacion_gpos_REM_NREM_',
                         toString(dur_chunk),
                         '_barra.eps'),
         path=g_dir,device='eps',
         width=8,height=5,unit='in')
}

setwd(g_dir)
write.csv(matriz2,file='completo_estacionariedad_crudo.csv',
          row.names=F)


# guardar para la tesis
ggplot(RES.REM,aes(x=Neuropsi,y=Proporcion,
                   shape=Grupo,color=Grupo))+
  ylab('Stationary epoche [%]') +
  labs(title='At REM')+
  labs(color=NULL,shape=NULL) +
  facet_wrap(~Canal,ncol=6) +
  theme_classic2() +
  #geom_smooth(method=lm,
  #            mapping=aes(x=Neuropsi,y=Proporcion),
  #            inherit.aes=F,
  #            se=F,color='black') +
  labs(color='Grupo')+
  stat_cor(data=RES.REM,inherit.aes=F,
           aes(x=Neuropsi,y=Proporcion),method='spearman') +
  #scale_y_continuous(expand=c(0,0))+
  scale_colour_discrete(guide = FALSE) +
  rotate_x_text(angle = 45)+
  theme(legend.position=c(1,1),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  geom_point()
ggsave(filename='/correlacion_estacionariedad_30_Neuropsi.png',path=dir_graf,
       device='png',units='cm',width=30,height=30,dpi=400)
ggsave(filename='/correlacion_estacionariedad_30_Neuropsi.eps',path=dir_graf,
       device='eps',units='cm',width=30,height=30,dpi=300)

ggplot(RES.REM,aes(x=Age,y=Proporcion,
                   shape=Grupo,color=Grupo))+
  ylab('Stationary epoche [%]') +
  labs(title='At REM')+
  labs(color=NULL,shape=NULL) +
  facet_wrap(~Canal,ncol=6) +
  theme_classic2() +
  #geom_smooth(method=lm,
  #            mapping=aes(x=Neuropsi,y=Proporcion),
  #            inherit.aes=F,
  #            se=F,color='black') +
  labs(color='Grupo')+
  stat_cor(data=RES.REM,inherit.aes=F,
           aes(x=Age,y=Proporcion),method='spearman') +
  #scale_y_continuous(expand=c(0,0))+
  scale_colour_discrete(guide = FALSE) +
  rotate_x_text(angle = 45)+
  theme(legend.position=c(1,1),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  geom_point()
ggsave(filename='/correlacion_estacionariedad_30_edad.png',path=dir_graf,
       device='png',units='cm',width=30,height=30,dpi=400)
ggsave(filename='/correlacion_estacionariedad_30_edad.eps',path=dir_graf,
       device='eps',units='cm',width=30,height=30,dpi=300)