set title "Autovectors per una caixa 1-D amb perturbació(N=500)"
set xlabel "x(Å)"
set ylabel "Φ"
set key below

#f(x)=sqrt(2)*sin(pi*x)
#g(x)=sqrt(2)*sin(2*pi*x)
#h(x)=sqrt(2)*sin(3*pi*x)
set xrange[-4:2]

plot  "apartat4.dat" index 2 u 1:2 w l t "E_{1}=-33.8976 eV",\
      "apartat3.dat" u 1:2 w l lw 2 t "β=5 eV",\
      "apartat3(2).dat" u 1:2 w l lw 2 t "β=10 eV"

 
pause -1

#Output
set term png 
set output "P8-2016-c2-fig.png"

replot
reset 	
exit
