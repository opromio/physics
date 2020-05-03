set title "Trajectòries"
set xlabel "x"
set ylabel "y"
set key below


plot  "PART1.dat" u 1:2 w l t "Partícula 1",\
      "PART2.dat" u 1:2 w l t "Partícula 2",\
      "PART3.dat" u 1:2 w l t "Partícula 3",\
      "PART4.dat" u 1:2 w l t "Partícula 4",\
      "PART5.dat" u 1:2 w l t "Partícula 5"

pause -1

#Output
set term png 
set output "P5-2016-fig2-c2.png"

replot
reset 	
exit
