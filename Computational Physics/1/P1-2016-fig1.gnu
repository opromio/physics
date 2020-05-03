set title "Figura 1"
set xlabel "N"
set ylabel "S"

set logscale y

set size ratio 1
set border
set key center

plot "P1-2016-res1.dat" u 1:2 w l t"S@^4_N",\
"P1-2016-res1.dat" u 1:3 t"S@^{asim}_N"  w l
pause -1
set term png 
set output "P1-2016-fig1.png"

replot
reset 	
exit
