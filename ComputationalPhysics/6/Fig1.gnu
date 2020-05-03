set title "Convergència I_{1} i I_{2}"
set xlabel "N"
set ylabel "Error"
set key below

set logscale y
set logscale x
set format x "10^{%T}"
set format y "10^{%T}"
set xrange[1000:1000000]
plot  "Fig1.dat" u 1:2 w l t "Error estimat I_{1}",\
      "Fig1.dat" u 1:3 w l t "Error real I_{1}",\
      "Fig1.dat" u 1:4 w l t "Error estimat I_{2}",\
      "Fig1.dat" u 1:5 w l t "Error real I_{2}"

pause -1

#Output
set term png 
set output "P6-2016-c2-fig1.png"

replot
reset 	
exit
