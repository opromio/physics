#Titulo
set title "Figura 3"

#Titulos ejes
set xlabel "Posició pistó 1 (cm)"
set ylabel "X_i(cm)"

#Tamaño, bordes
set size ratio 1
set border

#PLOT--> Interpolacion (con linea) + t,x2 (con puntos)
plot "P2-2016-res2.dat" u 1:2 t"Interpolació"  w l,\
"P2-2016-res1.dat" u 1:3 t"Pistó 2"  w p
pause -1

#Output
set term png 
set output "P2-2016-fig3.png"

replot
reset 	
exit

