#Titulo
#set title "x^3-(87/10)x^2 +(2439/100)x -(21793/1000)" 

#Titulos ejes

#Leyenda, tamaño y bordes
set border
set key below
set xzeroaxis
set yzeroaxis
#set xrange[0:4]
#set yrange[:30]


#PLOT-->
plot "Fig2-2p54.dat" u 1:2 w lp t "2.54", "Fig2-2p55.dat" u 1:2 w lp t "2.55",\
"Fig2-2p7.dat" u 1:2 w lp t "2.7"

pause -1

#Output
set term png 
set output "P4-2016-fig2-c2.png"

replot
reset 	
exit
