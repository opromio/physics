set term png
set output "P9-2016-fig1-c2.png"
set xzeroaxis 
set yzeroaxis 
set title "Convergència en el punt (x,y)=(25.5,13.5) desde T_{int} = 15 ºC amb tol < 10^{-3}"
set xlabel "Número d'iteracions"
set ylabel "T(ºC)"
set xrange[0:8200]
set yrange[0:120]
set key right bottom 

p "G15.dat" u 1:2 w l lw 2 t "Gauss-Seidel", "J15.dat" u 1:2 w l lw 2 t"Jacobi", "SR15.dat" u 1:2 w l lw 2 t"Sobrerelaxació"
