#Titulo
set title "Figura 1"

#Titulos ejes
set xlabel "Error"
set ylabel "h"

#Leyenda, tamaño y bordes
set size ratio 1
set border
set key right

#PLOT-->Error cometido en funcion de H
plot "P3-2016-res2-c2.dat" u 1:4 w l 

pause -1

#Output
set term png 
set output "P3-2016-fig1-c2.png"

replot
reset 	
exit
