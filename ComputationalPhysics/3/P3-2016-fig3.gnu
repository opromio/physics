#Titulo
set title "Comparació errors canvi de variable"

#Titulos ejes
set xlabel "h(m)"
set ylabel "Error(Kg)"
set logscale y
set logscale x
set format x "10^{%T}"
set format y "10^{%T}"

#Leyenda, tamaño y bordes

#set size ratio 1
set border
set key below

set xrange[:1]
#set yrange[:1]

#PLOT-->Error cometido en funcion de H
plot  "P3-Erroresf2.dat" u 1:2 w lp t "(sense)Trapezis",\
"P3-Erroresf2.dat" u 1:3 w lp t "(sense)Simpson",\
"P3-Erroresf3.dat" u 1:2 w lp t "(amb)Trapezis",\
"P3-Erroresf3.dat" u 1:3 w lp t "(amb)Simpson"


pause -1

#Output
set term png 
set output "P3-2016-fig3-c2.png"

replot
reset 	
exit
