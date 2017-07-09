fr = csvread('ej_priestley_f.csv');
fr = fr(:,2);

L = length(fr)-1
fr = fr(1:L);

yy = csvread('ej_priestley_y.csv');

K = length(yy(:,1))-1
yy = yy(2:K,1:L);

ind = 1:(K-1)

###################################3

#
#surf(ind,fr,yy(ind,:)');
#ylabel('Frecuencia normalizada')
#

figure(2,[0 , 0 , 640 , 480 ])

# usando la longitud de la serie
longitud = 512*6;
t_max = 6;
surf(ind*(t_max/(K-1)),fr*longitud,yy(ind,:)');

ylabel('1 / Frecuencia')
xlabel('Tiempo')
#title('Espectro de Priestley, EEG')

view(135,30)

axis( [ (t_max/(K-1)) t_max longitud*fr(1) longitud*fr(L)])

print('-S640,480', 'ejemplo_espectro.pdf')