set title "Figura 2"

set xlabel "N"
set ylabel "S^4/S_{asi}"

set size ratio 1
set border

plot "P1-2016-res1.dat" u 1:4 w p t("(Q=S@^8_N)/S@^{asim}_N")
pause -1
set term png 
set output "P1-2016-fig1.png"

replot
reset 	
exit

