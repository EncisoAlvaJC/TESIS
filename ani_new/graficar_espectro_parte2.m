# directorio de trabajo
cd 'C:/Users/EQUIPO 1/Desktop/Julio';

fr = csvread('L4i_frecuencias.csv');
fr = fr(:,2);

yy = csvread('L4i_espectro.csv');
yy = yy(1:22,1:11319);

# una anomalia con el bloque 0
ind = 2:22;

# con frecuencia normalizada
surf(ind,fr,yy(ind,:)');
ylabel('Frecuencia normalizada')
# usando la longitud de la serie
#longitud = 2852216;
#surf(ind,fr*longitud,yy');
#ylabel('Frecuencia')

title('Espectro de Priestley, EEG')
xlabel('Indice de bloque (tiempo)')

view(135,30)
view(135,50)