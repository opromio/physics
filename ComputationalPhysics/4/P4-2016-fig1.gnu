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


g(x)=((-21793 + 24390*x - 8700*(x**2) + 1000*(x**3))*(cosh(x)/1000))
h(x)=((2439/100) - (87*x)/5 + 3*x**2)*cosh(x) + sinh(x)*(-(21793/1000) + (2439*x)/100 - (87*x**2)/10 + x**3) 


#PLOT-->Error cometido en funcion de H
plot g(x) w l t "G(x)=cosh(x)f(x)", h(x) w l t "dG(x)"

pause -1

#Output
set term png 
set output "P4-2016-fig1-c2.png"

replot
reset 	
exit
