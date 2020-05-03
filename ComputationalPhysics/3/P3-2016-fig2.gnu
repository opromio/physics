#Titulo
#set title "Errors en el calcul de la massa en funció del pas d'integració"

#Titulos ejes
set xlabel "h(10^6 Km)"
set ylabel "Error(10^6 Km)"
set logscale y
set logscale x
set format x "10^{%T}"
set format y "10^{%T}"

#Leyenda, tamaño y bordes

#set size ratio 1
set border
set key below

f(x)=x**2
g(x)=x**4
#set xrange[:1]
#set yrange[:1]f(x) w lp, g(x) w lp,

#PLOT-->Error cometido en funcion de H
plot   "P3-Erroresf2.dat" u 1:2 w lp t "Trapezis",\
"P3-Erroresf2.dat" u 1:3 w lp t "Simpson"


pause -1

#Output
set term png 
set output "P3-2016-c2-fig2.png"

replot
reset 	
exit
