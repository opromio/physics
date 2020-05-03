set title "Evolució θ(t) en x_i= 5, 35, 115 (cm)"
set xlabel "t(s)"
set ylabel "θ(ºC)"
set key left top


plot "Evolucio.dat" u 1:2 w l lw 3 t"x=0.05 m",\
     "" u 1:3 w l lw 3 t"x=0.35 m",\
     "" u 1:4 w l lw 3 t"x=1.15 m",\



pause -1

#Output
set term png 
set output "P10-2016-fig1-c2.png"

replot
reset 	
exit

