set title "Pèndul simple per oscil·lacions grans"
set xlabel "t(s)"
set ylabel "Theta(rad)"
set key below

plot  "P7-2016-res.dat" index 2 u 1:2 w l t "Euler",\
      "P7-2016-res.dat" index 3 u 1:2 w l t "Euler millorat",\
      
pause -1

#Output
set term png 
set output "P7-2016-c2-fig2.png"

replot
reset 	
exit
