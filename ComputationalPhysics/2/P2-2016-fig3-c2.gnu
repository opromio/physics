#Titulo
set title "Figura 3"

#Titulos ejes
set xlabel "Posició pistó 4 (cm)"
set ylabel "X_i(cm)"

#Definim el rang en que comparem
set xrange [:6]

#Tamaño, bordes
set size ratio 1
set border

#PLOT--> Interpolacion (con linea) + t,x2 (con puntos)
plot "P2-2016-res2-c2.dat" u 1:2 t"Interpolació grau 0"  w l,\
"P2-2016-res2-c2.dat" u 1:3 t"Interpolació lineal"  w l,\
"P2-2016-res1-c2.dat" u 1:5 t"Pistó 4"  w p
pause -1

#Output
set term png 
set output "P2-2016-fig3-c2.png"

replot
reset 	
exit

