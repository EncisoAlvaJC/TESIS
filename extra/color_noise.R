# este script genera sennales que corresponden a varios
# colores de ruido, calcula su espectro de potencias y
# grafica en escala log-log
#
# usar sennales artificiales lo hace lucir mas 'realista'
# que graficar lo que son, analiticamente, los colores

# directorio de guardado, cambiar
#setwd('~/TESIS/TESIS/extra')

# librerias para generar los ruidos
require(tuneR)
require(psd)

# colores para generar los ruidos, y su color grafico
col.exp = c(0,1,2,-1,-2)
col.col = c('gray','pink','red','blue','violet')

n.colores = length(col.exp)

# parametros: fr de muestreo (Hz) y duracion de la sennal (s)
f.muestreo  = 2*(10**4)
t.sennal    = 1

# contenedor de datos
esp = matrix(0,nrow=n.colores,
             ncol=t.sennal*f.muestreo/2)

# generacion de datos artificiales, usando el paquete tuneR
set.seed(2017)
for(k in 1:n.colores){
  sennal = noise(kind='power',alpha=col.exp[k],
                 duration=t.sennal,samp.rate=f.muestreo,
                 xunit='time')
  sennal.v = attr(sennal,'left')
  
  # el espectro se calcula usando el paquete psd
  espect   = pspectrum(sennal.v,Nyquist.normalize=F,
                       x.frqsamp=f.muestreo)
  esp[k,] = log(espect$spec)
}

# vector con las frecuencias
frec.v = espect$freq

# este paso equivale a usar el espectro normalizado
for(k in 1:n.colores){
  esp[k,] = esp[k,] - esp[k,1]
}


#######################################
# grafico usando plot

# parametros graficos
mmax = max(esp)
mmin = min(esp)

# grafico vacio
plot(0,type='n',
     xlim=c(1,log(max(frec.v),10)),ylim=c(mmin,mmax),
     xlab = 'Frecuencia (Hz)',
     ylab='Densidad de Potencia Espectral (dB)',
     main='Ruidos de color',
     xaxt='n',las=2)
axis(1,at=0:10,label=10**(0:10))

# marcas logaritmicas de ejes
axis(1,at=log((1:10)*(10**0),10),label=F)
axis(1,at=log((1:10)*(10**1),10),label=F)
axis(1,at=log((1:10)*(10**2),10),label=F)
axis(1,at=log((1:10)*(10**3),10),label=F)

# lineas per se de los espectros
for(k in 1:n.colores){
  lines(log(frec.v,10),esp[k,],
        type='l',col=col.col[k],lwd=2)
}

#######################################
# grafico usando ggplot
require(ggplot2)
require(tidyr)
require(scales)

# arreglo de los datos
spec.d = data.frame(blanco  =esp[1,],
                    rosa    =esp[2,],
                    rojo    =esp[3,],
                    azul    =esp[4,],
                    violeta =esp[5,],
                    Frecuencia = frec.v)

# parche: evita error cuando se escala en log la frecuencia,
# y por estetica para evitar el efecto de puntos no-equiespaciados
spec.2 = subset(spec.d,Frecuencia>10)

spec.2 %>%
  gather(Color,Espectro, blanco,rosa,rojo,azul,violeta) %>%
  ggplot(aes(x=Frecuencia, y=Espectro, colour=Color)) +
  geom_line() +
  scale_x_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x)))+
  scale_color_manual(values=c('blue','black','red','pink','violet'))
