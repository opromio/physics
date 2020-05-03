set title "Figura 1"
set xlabel "N"
set ylabel "S"

set terminal postscript eps enhanced
set key spacing 1.3

set logscale y

set term png 
set output "P1-2016-fig1.png"

set size ratio 1
set border

unset key

plot "P1-2016-res1.dat" u 1:2 w p t"AS",\
"P1-2016-res1.dat" u 1:3 t"HOLA" w p 

reset 	
exit
