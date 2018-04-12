# leer las variables

if(TRUE){
  edad.ctrl = scan()
  edad.pdcl = scan()
  
  escol.ctrl = scan()
  escol.pdcl = scan()
  
  neuropsi.ctrl = scan()
  neuropsi.pdcl = scan()
  
  mmse.ctrl = scan()
  mmse.pdcl = scan()
  
  sast.ctrl = scan()
  sast.pdcl = scan()
  
  gds.ctrl = scan()
  gds.pdcl = scan()
  
  total.ctrl = scan()
  total.pdcl = scan()
  
  mor.ctrl = scan()
  mor.pdcl = scan()
  
  porc.ctrl = scan()
  porc.pdcl = scan()
}

if(FALSE){
###
# edad
mean(edad.ctrl)
sd(edad.ctrl)

mean(edad.pdcl)
sd(edad.pdcl)

print('Edad')

C = c(mean(edad.ctrl),sd(edad.ctrl),
      NA,
      mean(edad.pdcl),sd(edad.pdcl))
View(t(C))
invisible(readline(prompt="Presion [enter] para continuar"))

a = wilcox.test(edad.ctrl,edad.pdcl,paired = F,exact = T,
                correct = T,conf.int = T)
tmp = c(a$p.value,a$statistic,a$estimate,a$conf.int)
View(t(tmp))
invisible(readline(prompt="Presion [enter] para continuar"))

b = t.test(edad.ctrl,edad.pdcl,paired=F)
tm2 = c(b$p.value,b$statistic,b$parameter,b$estimate,b$conf.int)
View(t(tm2))
invisible(readline(prompt="Presion [enter] para continuar"))

###
# escolaridad

mean(escol.ctrl)
sd(escol.ctrl)

mean(escol.pdcl)
sd(escol.pdcl)

print('Escolaridad')

C = c(mean(escol.ctrl),sd(escol.ctrl),
      NA,
      mean(escol.pdcl),sd(escol.pdcl))
View(t(C))
invisible(readline(prompt="Presion [enter] para continuar"))

a = wilcox.test(escol.ctrl,escol.pdcl,paired = F,exact = T,
                correct = T,conf.int = T)
tmp = c(a$p.value,a$statistic,a$estimate,a$conf.int)
View(t(tmp))
invisible(readline(prompt="Presion [enter] para continuar"))

b = t.test(escol.ctrl,escol.pdcl,paired=F)
tm2 = c(b$p.value,b$statistic,b$parameter,b$estimate,b$conf.int)
View(t(tm2))
invisible(readline(prompt="Presion [enter] para continuar"))

###
# neuropsi
mean(neuropsi.ctrl)
sd(neuropsi.ctrl)

mean(neuropsi.pdcl)
sd(neuropsi.pdcl)

print('Neuropsi')

C = c(mean(neuropsi.ctrl),sd(neuropsi.ctrl),
      NA,
      mean(neuropsi.pdcl),sd(neuropsi.pdcl))
View(t(C))
invisible(readline(prompt="Presion [enter] para continuar"))

a = wilcox.test(neuropsi.ctrl,neuropsi.pdcl,paired = F,exact = T,
                correct = T,conf.int = T)
tmp = c(a$p.value,a$statistic,a$estimate,a$conf.int)
View(t(tmp))
invisible(readline(prompt="Presion [enter] para continuar"))

b = t.test(neuropsi.ctrl,neuropsi.pdcl,paired=F)
tm2 = c(b$p.value,b$statistic,b$parameter,b$estimate,b$conf.int)
View(t(tm2))
invisible(readline(prompt="Presion [enter] para continuar"))

###
# mmse
mean(mmse.ctrl)
sd(mmse.ctrl)

mean(mmse.pdcl)
sd(mmse.pdcl)

print('MMSE')

C = c(mean(mmse.ctrl),sd(mmse.ctrl),
      NA,
      mean(mmse.pdcl),sd(mmse.pdcl))
View(t(C))
invisible(readline(prompt="Presion [enter] para continuar"))

a = wilcox.test(mmse.ctrl,mmse.pdcl,paired = F,exact = T,
                correct = T,conf.int = T)
tmp = c(a$p.value,a$statistic,a$estimate,a$conf.int)
View(t(tmp))
invisible(readline(prompt="Presion [enter] para continuar"))

b = t.test(mmse.ctrl,mmse.pdcl,paired=F)
tm2 = c(b$p.value,b$statistic,b$parameter,b$estimate,b$conf.int)
View(t(tm2))
invisible(readline(prompt="Presion [enter] para continuar"))

###
# sast
mean(sast.ctrl,na.rm = T)
sd(sast.ctrl,na.rm = T)

mean(sast.pdcl,na.rm = T)
sd(sast.pdcl,na.rm = T)

print('SAST')

C = c(mean(sast.ctrl),sd(sast.ctrl),
      NA,
      mean(sast.pdcl),sd(sast.pdcl))
View(t(C))
invisible(readline(prompt="Presion [enter] para continuar"))

a = wilcox.test(sast.ctrl,sast.pdcl,paired = F,exact = T,
                correct = T,conf.int = T)
tmp = c(a$p.value,a$statistic,a$estimate,a$conf.int)
View(t(tmp))
invisible(readline(prompt="Presion [enter] para continuar"))

b = t.test(sast.ctrl,sast.pdcl,paired=F)
tm2 = c(b$p.value,b$statistic,b$parameter,b$estimate,b$conf.int)
View(t(tm2))
invisible(readline(prompt="Presion [enter] para continuar"))

###
# gds
mean(gds.ctrl)
sd(gds.ctrl)

mean(gds.pdcl)
sd(gds.pdcl)

print('GDS')

C = c(mean(gds.ctrl),sd(gds.ctrl),
      NA,
      mean(gds.pdcl),sd(gds.pdcl))
View(t(C))
invisible(readline(prompt="Presion [enter] para continuar"))

a = wilcox.test(gds.ctrl,gds.pdcl,paired = F,exact = T,
                correct = T,conf.int = T)
tmp = c(a$p.value,a$statistic,a$estimate,a$conf.int)
View(t(tmp))
invisible(readline(prompt="Presion [enter] para continuar"))

b = t.test(gds.ctrl,gds.pdcl,paired=F)
tm2 = c(b$p.value,b$statistic,b$parameter,b$estimate,b$conf.int)
View(t(tm2))
invisible(readline(prompt="Presion [enter] para continuar"))

###
# total
mean(total.ctrl)
sd(total.ctrl)

mean(total.pdcl)
sd(total.pdcl)

print('Sueno total')

C = c(mean(total.ctrl),sd(total.ctrl),
      NA,
      mean(total.pdcl),sd(total.pdcl))
View(t(C))
invisible(readline(prompt="Presion [enter] para continuar"))

a = wilcox.test(total.ctrl,total.pdcl,paired = F,exact = T,
                correct = T,conf.int = T)
tmp = c(a$p.value,a$statistic,a$estimate,a$conf.int)
View(t(tmp))
invisible(readline(prompt="Presion [enter] para continuar"))

b = t.test(total.ctrl,total.pdcl,paired=F)
tm2 = c(b$p.value,b$statistic,b$parameter,b$estimate,b$conf.int)
View(t(tm2))
invisible(readline(prompt="Presion [enter] para continuar"))

###
# mor
mean(mor.ctrl)
sd(mor.ctrl)

mean(mor.pdcl)
sd(mor.pdcl)

print('MOR tiempo')

C = c(mean(mor.ctrl),sd(mor.ctrl),
      NA,
      mean(mor.pdcl),sd(mor.pdcl))
View(t(C))
invisible(readline(prompt="Presion [enter] para continuar"))

a = wilcox.test(mor.ctrl,mor.pdcl,paired = F,exact = T,
                correct = T,conf.int = T)
tmp = c(a$p.value,a$statistic,a$estimate,a$conf.int)
View(t(tmp))
invisible(readline(prompt="Presion [enter] para continuar"))

b = t.test(mor.ctrl,mor.pdcl,paired=F)
tm2 = c(b$p.value,b$statistic,b$parameter,b$estimate,b$conf.int)
View(t(tm2))
invisible(readline(prompt="Presion [enter] para continuar"))

###
# porc
mean(porc.ctrl)
sd(porc.ctrl)

mean(porc.pdcl)
sd(porc.pdcl)

print('MOR porcentaje')

C = c(mean(porc.ctrl),sd(porc.ctrl),
      NA,
      mean(porc.pdcl),sd(porc.pdcl))
View(t(C))
invisible(readline(prompt="Presion [enter] para continuar"))

a = wilcox.test(porc.ctrl,porc.pdcl,paired = F,exact = T,
                correct = T,conf.int = T)
tmp = c(a$p.value,a$statistic,a$estimate,a$conf.int)
View(t(tmp))
invisible(readline(prompt="Presion [enter] para continuar"))

b = t.test(porc.ctrl,porc.pdcl,paired=F)
tm2 = c(b$p.value,b$statistic,b$parameter,b$estimate,b$conf.int)
View(t(tm2))
invisible(readline(prompt="Presion [enter] para continuar"))
}

##############################################################

edad = c(edad.ctrl,edad.pdcl)
escol = c(escol.ctrl,escol.pdcl)
neuropsi = c(neuropsi.ctrl,neuropsi.pdcl)
mmse = c(mmse.ctrl,mmse.pdcl)
sast = c(sast.ctrl,sast.pdcl)
gds = c(gds.ctrl,gds.pdcl)
total = c(total.ctrl,total.pdcl)
mor = c(mor.ctrl,mor.pdcl)
porc = c(porc.ctrl,porc.pdcl)

print('Edad')
A = cor.test(edad,edad,method = 'spearman')
B = t(c(A$estimate,A$p.value,A$statistic))
View(B)
invisible(readline(prompt="Presion [enter] para continuar"))
A = cor.test(edad,escol,method = 'spearman')
B = t(c(A$estimate,A$p.value,A$statistic))
View(B)
invisible(readline(prompt="Presion [enter] para continuar"))
A = cor.test(edad,neuropsi,method = 'spearman')
B = t(c(A$estimate,A$p.value,A$statistic))
View(B)
invisible(readline(prompt="Presion [enter] para continuar"))
A = cor.test(edad,mmse,method = 'spearman')
B = t(c(A$estimate,A$p.value,A$statistic))
View(B)
invisible(readline(prompt="Presion [enter] para continuar"))
A = cor.test(edad,sast,method = 'spearman')
B = t(c(A$estimate,A$p.value,A$statistic))
View(B)
invisible(readline(prompt="Presion [enter] para continuar"))
A = cor.test(edad,gds,method = 'spearman')
B = t(c(A$estimate,A$p.value,A$statistic))
View(B)
invisible(readline(prompt="Presion [enter] para continuar"))
A = cor.test(edad,total,method = 'spearman')
B = t(c(A$estimate,A$p.value,A$statistic))
View(B)
invisible(readline(prompt="Presion [enter] para continuar"))
A = cor.test(edad,mor,method = 'spearman')
B = t(c(A$estimate,A$p.value,A$statistic))
View(B)
invisible(readline(prompt="Presion [enter] para continuar"))
A = cor.test(edad,porc,method = 'spearman')
B = t(c(A$estimate,A$p.value,A$statistic))
View(B)
invisible(readline(prompt="Presion [enter] para continuar"))

print('Edad')
A = cor.test(escol,escol,method = 'spearman')
B = t(c(A$estimate,A$p.value,A$statistic))
View(B)
invisible(readline(prompt="Presion [enter] para continuar"))
A = cor.test(escol,neuropsi,method = 'spearman')
B = t(c(A$estimate,A$p.value,A$statistic))
View(B)
invisible(readline(prompt="Presion [enter] para continuar"))
A = cor.test(escol,mmse,method = 'spearman')
B = t(c(A$estimate,A$p.value,A$statistic))
View(B)
invisible(readline(prompt="Presion [enter] para continuar"))
A = cor.test(escol,sast,method = 'spearman')
B = t(c(A$estimate,A$p.value,A$statistic))
View(B)
invisible(readline(prompt="Presion [enter] para continuar"))
A = cor.test(escol,gds,method = 'spearman')
B = t(c(A$estimate,A$p.value,A$statistic))
View(B)
invisible(readline(prompt="Presion [enter] para continuar"))
A = cor.test(escol,total,method = 'spearman')
B = t(c(A$estimate,A$p.value,A$statistic))
View(B)
invisible(readline(prompt="Presion [enter] para continuar"))
A = cor.test(escol,mor,method = 'spearman')
B = t(c(A$estimate,A$p.value,A$statistic))
View(B)
invisible(readline(prompt="Presion [enter] para continuar"))
A = cor.test(escol,porc,method = 'spearman')
B = t(c(A$estimate,A$p.value,A$statistic))
View(B)
invisible(readline(prompt="Presion [enter] para continuar"))

print('Neuropsi')
A = cor.test(neuropsi,neuropsi,method = 'spearman')
B = t(c(A$estimate,A$p.value,A$statistic))
View(B)
invisible(readline(prompt="Presion [enter] para continuar"))
A = cor.test(neuropsi,mmse,method = 'spearman')
B = t(c(A$estimate,A$p.value,A$statistic))
View(B)
invisible(readline(prompt="Presion [enter] para continuar"))
A = cor.test(neuropsi,sast,method = 'spearman')
B = t(c(A$estimate,A$p.value,A$statistic))
View(B)
invisible(readline(prompt="Presion [enter] para continuar"))
A = cor.test(neuropsi,gds,method = 'spearman')
B = t(c(A$estimate,A$p.value,A$statistic))
View(B)
invisible(readline(prompt="Presion [enter] para continuar"))
A = cor.test(neuropsi,total,method = 'spearman')
B = t(c(A$estimate,A$p.value,A$statistic))
View(B)
invisible(readline(prompt="Presion [enter] para continuar"))
A = cor.test(neuropsi,mor,method = 'spearman')
B = t(c(A$estimate,A$p.value,A$statistic))
View(B)
invisible(readline(prompt="Presion [enter] para continuar"))
A = cor.test(neuropsi,porc,method = 'spearman')
B = t(c(A$estimate,A$p.value,A$statistic))
View(B)
invisible(readline(prompt="Presion [enter] para continuar"))

print('MMSE')
A = cor.test(mmse,mmse,method = 'spearman')
B = t(c(A$estimate,A$p.value,A$statistic))
View(B)
invisible(readline(prompt="Presion [enter] para continuar"))
A = cor.test(mmse,sast,method = 'spearman')
B = t(c(A$estimate,A$p.value,A$statistic))
View(B)
invisible(readline(prompt="Presion [enter] para continuar"))
A = cor.test(mmse,gds,method = 'spearman')
B = t(c(A$estimate,A$p.value,A$statistic))
View(B)
invisible(readline(prompt="Presion [enter] para continuar"))
A = cor.test(mmse,total,method = 'spearman')
B = t(c(A$estimate,A$p.value,A$statistic))
View(B)
invisible(readline(prompt="Presion [enter] para continuar"))
A = cor.test(mmse,mor,method = 'spearman')
B = t(c(A$estimate,A$p.value,A$statistic))
View(B)
invisible(readline(prompt="Presion [enter] para continuar"))
A = cor.test(mmse,porc,method = 'spearman')
B = t(c(A$estimate,A$p.value,A$statistic))
View(B)
invisible(readline(prompt="Presion [enter] para continuar"))

print('SAST')
A = cor.test(sast,sast,method = 'spearman')
B = t(c(A$estimate,A$p.value,A$statistic))
View(B)
invisible(readline(prompt="Presion [enter] para continuar"))
A = cor.test(sast,gds,method = 'spearman')
B = t(c(A$estimate,A$p.value,A$statistic))
View(B)
invisible(readline(prompt="Presion [enter] para continuar"))
A = cor.test(sast,total,method = 'spearman')
B = t(c(A$estimate,A$p.value,A$statistic))
View(B)
invisible(readline(prompt="Presion [enter] para continuar"))
A = cor.test(sast,mor,method = 'spearman')
B = t(c(A$estimate,A$p.value,A$statistic))
View(B)
invisible(readline(prompt="Presion [enter] para continuar"))
A = cor.test(sast,porc,method = 'spearman')
B = t(c(A$estimate,A$p.value,A$statistic))
View(B)
invisible(readline(prompt="Presion [enter] para continuar"))

print('GDS')
A = cor.test(gds,gds,method = 'spearman')
B = t(c(A$estimate,A$p.value,A$statistic))
View(B)
invisible(readline(prompt="Presion [enter] para continuar"))
A = cor.test(gds,total,method = 'spearman')
B = t(c(A$estimate,A$p.value,A$statistic))
View(B)
invisible(readline(prompt="Presion [enter] para continuar"))
A = cor.test(gds,mor,method = 'spearman')
B = t(c(A$estimate,A$p.value,A$statistic))
View(B)
invisible(readline(prompt="Presion [enter] para continuar"))
A = cor.test(gds,porc,method = 'spearman')
B = t(c(A$estimate,A$p.value,A$statistic))
View(B)
invisible(readline(prompt="Presion [enter] para continuar"))

print('Sueno, total')
A = cor.test(total,total,method = 'spearman')
B = t(c(A$estimate,A$p.value,A$statistic))
View(B)
invisible(readline(prompt="Presion [enter] para continuar"))
A = cor.test(total,mor,method = 'spearman')
B = t(c(A$estimate,A$p.value,A$statistic))
View(B)
invisible(readline(prompt="Presion [enter] para continuar"))
A = cor.test(total,porc,method = 'spearman')
B = t(c(A$estimate,A$p.value,A$statistic))
View(B)
invisible(readline(prompt="Presion [enter] para continuar"))

print('MOR, tiempo')
A = cor.test(mor,mor,method = 'spearman')
B = t(c(A$estimate,A$p.value,A$statistic))
View(B)
invisible(readline(prompt="Presion [enter] para continuar"))
A = cor.test(mor,porc,method = 'spearman')
B = t(c(A$estimate,A$p.value,A$statistic))
View(B)
invisible(readline(prompt="Presion [enter] para continuar"))

print('MOR, porcentaje')
A = cor.test(porc,porc,method = 'spearman')
B = t(c(A$estimate,A$p.value,A$statistic))
View(B)
invisible(readline(prompt="Presion [enter] para continuar"))