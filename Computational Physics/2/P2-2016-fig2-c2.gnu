#Titulo
set title "Figura 2"
#Titulos ejes
set xlabel "Posició pistó 1 (cm)"
set ylabel "X_i(cm)"

#Tamaño y bordes
set size ratio 1
set border

#PLOT--> En este caso piston 3 y 5 en funcion del 1.
plot "P2-2016-res1-c2.dat" u 2:4 t"Pistó 3"  w l,\
"P2-2016-res1-c2.dat" u 2:6 t"Pistó 5"  w l
pause -1

#Output
set term png 
set output "P2-2016-fig2-c2.png"

replot
reset 	
exit

