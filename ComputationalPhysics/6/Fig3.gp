set xlabel "N"
set ylabel "Error"
set key below

set title "Convergència I_{3}" 


set logscale y
set logscale x
set format x "10^{%T}"
set format y "10^{%T}"
set xrange[1000:330000]
plot  "Fig2.dat" u 1:2 w l t "Error estimat",\
      "Fig2.dat" u 1:3 w l t "Error real",\
      

pause -1

#Output
set term png 
set output "P6-2016-c2-fig3.png"

replot
reset 	
exit
