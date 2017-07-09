# directorio
setwd('C:/Users/EQUIPO 1/Desktop/Julio')
# para leer los datos
library(R.matlab)
# contiene stationarity()
library(fractal)

# lee los datos y los convierte a vector
L4 = readMat('L4.mat');
L4 = unlist(L4)

# convierte a objeto tim.series para quitar tendencia y seasonality
L4t = ts(L4,frequency=5000);
s = stl(L4t,s.window='periodic')
L4rt = s$time.series[,'remainder']

# convierte el residual a vector, elimina informacion adicional
L4r = unclass(L4rt)
L4r = L4r[1:length(L4r)]

# calcula el espectro de Priestley y efectua prueba PSR
z = stationarity(L4r)

# extrae el esadistico usado en la prueba:
#    Y_{i,j} = log( f_{t_i}(omega_j) )
#    f_t(omega) = abs(A_t(omega))**2
# f es el estimador del modulo de la funcion de densidad espectral
# para la frecuencia omega en el tiempo t
# Y es el logaritmo del estimador f, en t y omega discretos
# t_i son los centros de los bloques equiespaciados (n.block)
# omega_t son las frecuencias, se guardan en z

y = z$anova
f = z$freq
# el anova contiene los Y_{i,j}
# freq guarda los indices de las frecuencias utilizadas, en el
# se describe que deben tener un cierto 'ancho de banda'

write.csv(y,'L4i_espectro.csv')
write.csv(f,'L4i_frecuencias.csv')

# la graficacion per se la hare en octave, porque es lo que a mi
# se me hizo mas facil y por ninguna otra razon en especial