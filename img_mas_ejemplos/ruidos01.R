###########################################################
## Ruidos de colores #####################################

# la libreria tuneR es para procesamiento de audio, creo
require('tuneR')

# para que lo aleatorio sea repetible
set.seed(2018)

# power se interpreta como 'ruido tipo f**(-power)'
# aqui van 2 segundos a 500 Hz
# el xunit es la unidad (unidades de tiempo / puntos)
a = noise(kind=c('power'),alpha = 1,duration = 2,samp.rate = 500,
          xunit='time')

# toma solo los datos
pin = a@left

# eso es todo
plot(pin,type='l', main='Ruido rosa')
