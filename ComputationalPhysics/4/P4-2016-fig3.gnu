#Titulo
#set title "x^3-(87/10)x^2 +(2439/100)x -(21793/1000)" 

#Titulos ejes

#Leyenda, tamaño y bordes
set border
set key below
set xzeroaxis
set yzeroaxis
set xrange[0:4]
set yrange[:50]

h(x)=((2439/100) - (87*x)/5 + 3*x**2)*cosh(x) + sinh(x)*(-(21793/1000) + (2439*x)/100 - (87*x**2)/10 + x**3) 

#PLOT-->
plot h(x) w l t"dF(x)", "p4-2016-res3-c2-n200.dat" u 1:4 w lp t "Der. Exac", "p4-2016-res3-c2-n10.dat" u 1:3 w p t "N_{iter}=10",\
"p4-2016-res3-c2-n200.dat" u 1:3 w p t "N_{iter}=200"

pause -1

#Output
set term png 
set output "P4-2016-fig3-c2.png"

replot
reset 	
exit
