set title "Convergència del mètode de la secant"
set xlabel "N"
set ylabel "Energia(eV)"
set key below



plot  "p8-2016-res.dat" index 0 u 1:2 w lp t "Autovalor de n=1",\
      "" index 1 u 1:2 w lp t "Autovalor de n=2",\
      "" index 2 u 1:2 w lp t "Autovalor de n=3"

 
pause -1

#Output
set term png 
set output "P8-2016-c2-fig.png"

replot
reset 	
exit
