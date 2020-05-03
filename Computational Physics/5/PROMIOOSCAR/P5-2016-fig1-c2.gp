set title "Funció Gaussiana"
set xlabel "x"
set ylabel "Densitat de probabilitat"
set key below

f(x)=(1/sqrt(2*pi))*exp(-(x)**2/2)

#plot f(x) t "Gauss", "P5-2016-c2-res1.dat" u 1:2:3 w e t "Box-Müller"

plot f(x) w l lt 4 lw 2 lc rgb "#DC143C" t"Gauss",\
"P5-2016-c2-res1.dat" u 1:2 lt 2 lc rgb "#00CED1" t"Box-Müller" with histeps,\
"P5-2016-c2-res1.dat" u 1:2:3 lt 2 lc rgb "blue" t"Error" with errorbars 
pause -1

#Output
set term png 
set output "P5-2016-fig1-c2.png"

replot
reset 	
exit
