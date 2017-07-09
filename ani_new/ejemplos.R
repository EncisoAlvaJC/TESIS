
#  generar N_t : normal estandar
set.seed(2016)
N = rnorm(1026,0,1)
N.t = ts(N,frequency = 512,start=c(0,0))

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

#  generar UM_t : proceso uniformemente modulado
set.seed(2016)
e = rnorm(1028,0,10**2)
Y = e
for(t in 3:1028){ 
  Y[t] = 0.8*Y[t-1] -0.4*Y[t-2] + e[t]
}
Y = Y[3:1028]
t = 1:1026
UM = (2-exp(-((t-500)**2)/(2*(200**2))))*Y
UM.t = ts(UM,frequency=512,start=c(0,0))

# graficar las puras series
dev.off()
par(mfrow=c(2,2),mar=c(2,3,4,1)+0.1)
plot(N.t,xlab='',main='Normal estandar (va iid)')
#plot(C.t,xlab='',
#     main= expression(symbol("c")^2))
plot(C.t,xlab='', main= '(7) normalizada (va iid)')
plot(R.t,xlab='', main= 'Caminata aleatoria normal')
plot(UM.t,xlab='', main= 'Proceso Uniformemente Modulado')


############################################################################
############################################################################


S.N = stl(N.t,s.window='periodic',robust=TRUE)
S.C = stl(C.t,s.window='periodic',robust=TRUE)
S.R = stl(R.t,s.window='periodic',robust=TRUE)
S.UM = stl(UM.t,s.window='periodic',robust=TRUE)

plot(S.N,main='Normal estandar (va iid)')

dev.off()
plot(S.N)
title(main ='Normal estandar (va iid)',cex.main = 2)
#plot(S.N,main='Normal estandar (va iid)',cex.main=3)

#Ns.t = S.N$time.series[,'seasonal']
#Cs.t = S.C$time.series[,'seasonal']
#Rs.t = S.R$time.series[,'seasonal']
#UMs.t = S.UM$time.series[,'seasonal']

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
