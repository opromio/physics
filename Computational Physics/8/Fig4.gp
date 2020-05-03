set title "Autovectors per una caixa 1-D amb perturbació(N=500)"
set xlabel "x(Å)"
set ylabel "Φ"
set key below


#set xrange[-4:2]
set yrange[0:1]

plot  "apartat4.dat" index 0 u 1:2 w l t "E_{1}=-48.1859 eV",\
      "apartat3.dat" u 1:2 w l t "β=5 eV",\
      "apartat3(2).dat" u 1:2 w l t "β=10 eV"

pause -1

#Output
set term png 
set output "P8-2016-c2-fig.png"

replot
reset 	
exit
