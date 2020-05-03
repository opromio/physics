set term png
set output "P9-3D.png"
set xzeroaxis 
set yzeroaxis
set zzeroaxis
set title "MAPA 3D TEMPERATURES"
set contour surface
set cntrparam levels auto 10
set view 45,30
set xlabel "x"
set ylabel "y"
set zlabel "T(ºC)"
set ticslevel 0
set xrange[0:40]
set zrange[0:80]
set yrange[0:27.5]
set palette rgbformulae 30,31,32
sp "3DMAP.dat" w pm3d