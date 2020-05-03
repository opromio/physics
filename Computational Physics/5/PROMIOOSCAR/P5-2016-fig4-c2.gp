set title "Histograma distàncies"
set xlabel "distància"
set ylabel "Densitat de probabilitat"
set key below


plot "P5-2016-c2-res3.dat" u 1:2 lt 2 lc rgb "#00CED1" t"Distància" with histeps,\
"P5-2016-c2-res3.dat" u 1:2:3 lt 2 lc rgb "blue" t"Error" with errorbars 
pause -1

#Output
set term png 
set output "P5-2016-fig4-c2.png"

replot
reset 	
exit