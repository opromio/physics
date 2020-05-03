set title "Figura 2"

set xlabel "N"
set ylabel "S^4/S_{asi}"

set term png 
set output "P1-2016-fig2.png"

set size ratio 1
set border

unset key

plot "P1-2016-res1.dat" u 1:4 w l

exit
