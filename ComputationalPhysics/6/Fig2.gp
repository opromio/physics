set key below

set terminal postscript enhanced 
#Output 
set output "P6-2016-c2-fig2.png"

set encoding iso_8859_1 
set title "I_{2}=0.010007 \261 0.000007" 
set xlabel "x({/Symbol m}m)"
set ylabel "I_{2}"

set logscale x

set format x "10^{%T}"

set xrange [1000:]

plot  "Fig1.dat" u 1:4:5 w e t "Error estimat" 

      
pause -1
replot	
exit
