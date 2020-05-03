set term x11
unset logscale y
n=100 #number of intervals
max=8. #max value
min=0. #min value
width=(max-min)/n #interval width
#function used to map a value to the intervals
hist(x,width)=width*floor(x/width)+width/2.0
set xrange [min:max]
set yrange [0:]
#to put an empty boundary around the
#data inside an autoscaled graph.
#set offset graph 0.05,0.05,0.05,0.0
set xtics min,(max-min)/8,max
set boxwidth width*0.9
set style fill solid 0.5 #fillstyle
set title "Distribucio gaussiana"
set tics out nomirror
set xlabel "$x$"
set ylabel "Frequency"
#count and plot
plot "gauss.txt" u (hist($1,width)):(1.0) smooth freq w boxes lc rgb"green" notitle
