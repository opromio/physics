set title "Pèndul simple per petites oscil·lacions"
set xlabel "t(s)"
set ylabel "Theta(rad)"
set key below


plot  "P7-2016-res.dat" index 0 u 1:2 w l t "Euler",\
      "P7-2016-res.dat" index 1 u 1:2 w l t "Euler millorat",\
      
pause -1

#Output
set term png 
set output "P7-2016-c2-fig1.png"

replot
reset 	
exit
