set title "Evolució T(t) en x_i= 5, 10, 20 i 30"
set xlabel "t(s)"
set ylabel "T(s)"
set key right top

plot "Evolucio.dat" u 1:2 w l lw 3 t"x=5",\
     "" u 1:3 w l lw 3 t"x=10",\
     "" u 1:4 w l lw 3 t"x=20",\
     "" u 1:5 w l lw 3 t"x=30"


pause -1

#Output
set term png 
set output "P10-2016-fig1-c2.png"

replot
reset 	
exit

