set title "Evolució temperatura promig per diferents valors de K"
set xlabel "t(s)"
set ylabel "θ(ºC)"
set key right bottom

plot "KFE.dat" w l lw 3 t"K_{Fe}",\
     "KAU.dat" w l lw 3 t"K_{Au}",\
   


pause -1

#Output
set term png 
set output "P10-2016-fig2-c2.png"

replot
reset 	
exit

