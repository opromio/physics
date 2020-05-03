set title "θ(x) (estacionari)"
set xlabel "x(m)"
set ylabel "θ (ºC)"
set key left top
set xrange [0:1.3]

plot "Apartat1.dat" u 1:2 w l lw 3 t"β=0.002",\
     "Apartat1b.dat" u 1:2 w l lw 3 t"β=0.0002",\
     "Apartat1c.dat"u 1:2 w l lw 3 t"β=0.00005"
  


pause -1

#Output
set term png 
set output "P10-2016-fig-c2.png"

replot
reset 	
exit

