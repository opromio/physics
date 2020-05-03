#Titulo
set title "Figura 1"

#Titulos ejes
set xlabel "t(s)"
set ylabel "X(cm)"

#Leyenda, tamaño y bordes
set size ratio 1
set border
set key right

#PLOT-->La posición de los 4 pistones en funcion del tiempo
plot "P2-2016-res1.dat" u 1:2 w l t"Pistó 1",\
"P2-2016-res1.dat" u 1:3 t"Pistó 3"  w l,\
"P2-2016-res1.dat" u 1:4 t"Pistó 3"  w l,\
"P2-2016-res1.dat" u 1:5 t"Pistó 4"  w l
pause -1

#Output
set term png 
set output "P2-2016-fig1.png"

replot
reset 	
exit
