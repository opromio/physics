set title "Funcions d'ona per una caixa 1-D de mida L (N=500)"
set xlabel "x'(x/L)"
set ylabel "Φ"
set key inside left bottom left

f(x)=sqrt(2)*sin(pi*x)
g(x)=sqrt(2)*sin(2*pi*x)
h(x)=sqrt(2)*sin(3*pi*x)

plot  "apartat4.dat" index 0 u 1:2 w p t "n=1",\
      "apartat4.dat" index 1 u 1:2 w p t "n=2",\
      "apartat4.dat" index 2 u 1:2 w p t "n=3",\
      f(x) w l t "", g(x) w l t "", h(x) w l t ""
 
pause -1

#Output
set term png 
set output "P8-2016-c2-fig.png"

replot
reset 	
exit
