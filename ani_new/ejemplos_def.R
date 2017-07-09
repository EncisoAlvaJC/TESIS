############################################################################
############################################################################

#  generar C_t : chi cuadrada normalizada
set.seed(2016)
C = ( rchisq(1026,7) - 7 ) /sqrt(7)
C.t = ts(C,frequency = 512,start=c(0,0))

#  generar R_t : caminata aleatoria
set.seed(2016)
e = rnorm(1027,0,1)
R = e
for(t in 2:1027){ R[t] = e[t] + R[t-1] }
R.t = ts(R[2:1027],frequency = 512,start=c(0,0))

# graficar las puras series
dev.off()

pdf('ejemplos.pdf',width=11.744,height=5.544)

par(mfrow=c(2,1),mar=c(2,2,3,1)+0.1)
plot(C.t,xlab='',main='Chi cuadrada (va iid)')
plot(R.t,xlab='', main= 'Caminata aleatoria normal')

dev.off()

############################################################################
############################################################################

S.C = stl(C.t,s.window='periodic',robust=TRUE)
S.R = stl(R.t,s.window='periodic',robust=TRUE)

dev.off()
pdf('stl_1.pdf',width=5.872,height=5.544)
plot(S.C,main='Chi cuadrada (va iid)',cex=2)
dev.off()

dev.off()
pdf('stl_2.pdf',width=5.872,height=5.544)
plot(S.R,main='Caminata aleatoria',cex=2)
dev.off()

############################################################################
############################################################################

dev.off()
par(mfrow=c(2,2),mar=c(2,3,4,1)+0.1)
plot(N.t,xlab='',main='Normal estandar (va iid)')
plot(S.N$time.series[,'remainder'],main='Normal estandar, filtrada')
plot(C.t,xlab='', main= '(7) normalizada (va iid)')
plot(S.C$time.series[,'remainder'],main='(7) normalizada, filtrada')

dev.off()
par(mfrow=c(2,2),mar=c(2,3,4,1)+0.1)
plot(R.t,xlab='', main= 'Caminata aleatoria normal')
plot(S.R$time.series[,'remainder'],main='Caminata aleatoria, filtrada')
plot(UM.t,xlab='', main= 'Proceso Uniformemente Modulado')
plot(S.UM$time.series[,'remainder'],main='Proceso Uniformemente Modulado, filtrada')

plot(R.t,xlab='', main= 'Caminata aleatoria normal')
plot(UM.t,xlab='', main= 'Proceso Uniformemente Modulado')

############################################################################
############################################################################

library(tseries)

Normal_estandar=N
adf.test(Normal_estandar)

Chi_cuad=C
adf.test(Chi_cuad)

Caminata_aleatoria = R
adf.test(Caminata_aleatoria)
