set term png
set output "P9-2016-fig5-c2.png"
set xzeroaxis 
set yzeroaxis
set zzeroaxis
set title "Mapa temperatures sense fonts de calor"
#set contour surface
#set cntrparam levels auto 10
set size ratio -1
set view 45,30
set xlabel "x(cm)"
set ylabel "y(cm)"
set ticslevel 0
set xrange[0:44.5]
set yrange[0:32.5]
unset key
set palette rgbformulae 30,31,32
set view map
set cblabel "Temperatures (ºC)"

sp "3DMAP.dat" w pm3d t""