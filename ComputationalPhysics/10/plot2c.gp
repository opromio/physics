set term gif size 1000,600 animate delay 10 loop 0 optimize
set output "P10-2016-fig3-c2.gif"
datafile="Gif.dat"
stats datafile
numblo=STATS_blocks
set title "T(x,t)"
set xrange [0:35]
set yrange [0:60]
set xlabel "x(cm)"
set ylabel "T(x,t) (ºC)"
do for [i=0:numblo-2:10]{
set label 2 sprintf('Time: %9.3f (sec)', i*0.01) at 10,160 right front font 'Verdana, 12'
plot datafile index i u 2:3 w l lw 4 t "T(x,t)", "" index 0 u 2:3 t "T_0(x)" w l lw 3
unset label 2
}