#Titulo
set title "Diagrama P-V" 

#Titulos ejes
set xlabel "Volum"
set ylabel "Pol"
#Leyenda, tamaño y bordes
set border
set key left
set xzeroaxis
set yzeroaxis



#PLOT-->Error cometido en funcion de H
plot "teq94.dat" u 1:2 w lp t "Isoterma T=0.94"

pause -1

#Output
set term png 
set output "P4-2016-c2-fig2.png"

replot
reset 	
exit