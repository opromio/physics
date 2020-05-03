#Titulo
#set title "Errors càlcul longitud en funció del pas d'integració"

#Titulos ejes
set xlabel "h(10^6 Km)"
set ylabel "Error(10^6 km)"
set logscale y
set logscale x
set format x "10^{%T}"
set format y "10^{%T}"

#Leyenda, tamaño y bordes

#set size ratio 1
set border
set key below
#set grid

f(x)=x**2
g(x)=x**4
#set xrange[:2]
#set yrange[:1]    f(x) w lp t "x^{2}", g(x) w lp t "x^{4}",

#PLOT-->Error cometido en funcion de H
plot  "P3-Erroresf1.dat" u 1:2 w lp t "Trapezis",\
"P3-Erroresf1.dat" u 1:3 w lp t "Simpson"

pause -1

#Output
set term png 
set output "P3-2016-c2-fig1.png"

replot
reset 	
exit
