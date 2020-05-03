set title "Evolució variança en funció de t"
set xlabel "t"
set ylabel "Variança"
set key below

f(x)=2*0.0002*x

set yrange[0:0.0018]

plot f(x) w l t "Ajust D=0.0002", "P5-2016-c2-res2.dat" u 1:2 w l t "Variança"
 

pause -1

#Output
set term png 
set output "P5-2016-fig3-c2.png"

replot
reset 	
exit
