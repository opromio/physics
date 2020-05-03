set title "T(x)"
set xlabel "x(cm)"
set ylabel "T  (? C)"
set key right top

plot "Apartat1.dat" u 1:2 w l lw 3 t""
  


pause -1

#Output
set term png 
set output "P10-2016-fig-c2.png"

replot
reset 	
exit

