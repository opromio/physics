set term png
set output "P9-2016-fig3-c2.png"
set xzeroaxis 
set yzeroaxis 
set title "Convergència en el punt (x,y)=(25.5,13.5) desde T_{int} = 1280 ºC amb tol < 10^{-3}"
set xlabel "Número d'iteracions"
set ylabel "T(ºC)"
set xrange[0:11600]
#set yrange[0:120]
set key right top 
#set format x '10^{%T}'
#set format y '10^{%T}'
p "G1280.dat" u 1:2 w l lw 2 t "Gauss-Seidel", "J1280.dat" u 1:2 w l lw 2 t"Jacobi", "SR1280.dat" u 1:2 w l lw 2 t"Sobrerelaxació"
