set title "Autovectors per una caixa 1-D (N=500)"
set xlabel "x(Å)"
set ylabel "Φ"
set key below

#f(x)=sqrt(2)*sin(pi*x)
#g(x)=sqrt(2)*sin(2*pi*x)
#h(x)=sqrt(2)*sin(3*pi*x)
set xrange[-4:2]

plot  "apartat1.dat" u 1:2 w l t "E_{1}=-50 eV",\
      "apartat1.dat" u 1:3 w l t "E_{2}=-49.5 eV",\
      "apartat1.dat" u 1:4 w l t "E_{3}=-45 eV",\
      "apartat1.dat" u 1:5 w l t "E_{4}=-44 eV",\
 
pause -1

#Output
set term png 
set output "P8-2016-c2-fig.png"

replot
reset 	
exit
