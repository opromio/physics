set title "Pèndul simple a l'espai de fase per a grans oscil·lacions"
set xlabel "DTheta(rad/s)"
set ylabel "Theta(rad)"
set key below


plot  "P7-2016-res.dat" index 2 u 2:3 w l t "Euler",\
      "P7-2016-res.dat" index 3 u 2:3 w l t "Euler millorat",\
           
pause -1

#Output
set term png 
set output "P7-2016-c2-fig3.png"

replot
reset 	
exit
