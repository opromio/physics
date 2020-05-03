set title "Evolució temperatura promig per diferents valors de K"
set xlabel "t(s)"
set ylabel "T(s)"
set key left bottom

plot "K1.dat" w l lw 3 t"K=1",\
     "K5.dat" w l lw 3 t"K=5",\
     "K10.dat" w l lw 3 t"K=10"


pause -1

#Output
set term png 
set output "P10-2016-fig2-c2.png"

replot
reset 	
exit

