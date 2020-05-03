#Titulo
#set title "Diagrama P-V" 

#Titulos ejes
set xlabel "Volum"
set ylabel "Presió"
#Leyenda, tamaño y bordes
set border
set key below
set xzeroaxis
set yzeroaxis



#PLOT-->Error cometido en funcion de H
plot "Fig1.dat" u 1:2 w lp t "Isoterma T=0.93"

pause -1

#Output
set term png 
set output "P4-2016-c2-fig1.png"

replot
reset 	
exit
