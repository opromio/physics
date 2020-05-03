set title "Acceptació i rebuig"
set xlabel "x"
set ylabel "Densitat de probabilitat"

set yrange[0:1.4]
unset key

f(x)=4*x/3
g(x)=-(8*x/3) + 4


plot f(x), g(x),\
"AiR.dat" u 1:2 lt 2 lc rgb "#00CED1" t"Acceptació i Rebuig" with histeps,\
"AiR.dat" u 1:2:3 lt 2 lc rgb "blue" t"Error" with errorbars 
pause -1

#Output
set term png 
set output "P5-2016-fig2-c2.png"

replot
reset 	
exit