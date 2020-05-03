#FIGURA1################################################################
set title "Pèndul simple per petites oscil·lacions"
set term png
set output "P7-2016-fig1.png"
set xzeroaxis 
set yzeroaxis 
set xlabel "t(s)"
set ylabel "θ(rad)"
set xrange[0:9.05]
set yrange[-0.3:0.4]
set key below

f(x)=0.2*cos(x*sqrt(9.8/1.27))
p "P7-2016-res.dat" index 0 u 1:2 w l t"Euler Normal",\
  "" index 1 u 1:2 w l t"Euler Millorat",\
   f(x) w l t"θ(t)=0.2cos(ωt)"
   
#FIGURA2################################################################
set title "Pèndul simple per oscil·lacions grans"
set term png
set output "P7-2016-fig2.png"
set xzeroaxis
set yzeroaxis 
set xlabel "t(s)"
set ylabel "θ(rad)"
set xrange[0:9.05]
set yrange[-25:10]
set key below
p "P7-2016-res.dat" index 2 u 1:2 w l t"Euler Normal",\
  "" index 3 u 1:2 w l t"Euler Millorat"

#FIGURA3################################################################
set title "Pèndul simple a l'espai de fase per a grans oscil·lacions"
set term png
set output "P7-2016-fig3.png"
set xzeroaxis
set yzeroaxis 
set xlabel "θ(rad)"
set ylabel "dθ (rad/s)"
set xrange[-22.5:5]
set yrange[-6:8]
set key below
p "P7-2016-res.dat" index 2 u 2:3 w l t"Euler Normal",\
  "" index 3 u 2:3 w l t"Euler Millorat"
  
#FIGURA4################################################################
set term png
set output "P7-2016-fig4.png"
set multiplot layout 2, 1 title "Evolució E_{cin}, E_{pot} i E_{T}"
set xzeroaxis
set yzeroaxis 
set xlabel "t(s)"
set ylabel "E     (J)"
set xrange[0:9.05]
set yrange[-3:3]
set title "C.I.: θ(0)=1, dθ(0)=0"
set key below
p "P7-2016-res.dat" index 4 u 1:4 w l t "E_{cin} E.N.",\
  "" index 4 u 1:6 w l t "E_{T} E.N.",\
  "" index 5 u 1:4 w l t "E_{cin} E.M.",\
  "" index 5 u 1:6 w l t "E_{T} E.M."
  
set title "C.I.: θ(0)=π-0.001, dθ(0)=0"
set yrange[-2:9]
p "P7-2016-res.dat" index 6 u 1:4 w l t"E_{cin} E.N.",\
   "" index 6 u 1:6 w l t"E_{T} E.N.",\
   "" index 7 u 1:4 w l t"E_{cin} E.M.",\
   "" index 7 u 1:6 w l t"E_{T} E.M."

unset multiplot 

#FIGURA5################################################################
set title "Figura 5"
set term png
set output "P7-2016-fig5.png"
set xzeroaxis
set yzeroaxis 
set xlabel "θ(rad)"
set ylabel "dθ (rad/s)"
set xrange[-5:45]
set yrange[-6:8]
set key below
p "P7-2016-res.dat" index 8 u 2:3 w p t"Euler Normal",\
  "" index 9 u 2:3 w p t"Euler Millorat"
  
#FIGURA6################################################################  
unset title
set term png
set output "P7-2016-fig6.png"
set multiplot layout 2, 2 title "E_{T} per diferents valors de N_{DAT}"
set xzeroaxis
set yzeroaxis 
set xlabel "t(s)"
set ylabel "E(J)"
set xrange[0:23]
set yrange[3:5]
p "P7-2016-res.dat" index 10 u 1:6 w l t"N_{DAT}=500 pasos"
p "P7-2016-res.dat" index 11 u 1:6 w l t"N_{DAT}=1000 pasos"
p "P7-2016-res.dat" index 12 u 1:6 w l t"N_{DAT}=2000 pasos"
p "P7-2016-res.dat" index 13 u 1:6 w l t"N_{DAT}=20000 pasos"
unset multiplot

#FIGURA7################################################################
set term png
set output "P7-2016-fig7.png"
set multiplot layout 2, 2 title "Convergència en funció de Δt"
unset title
set xzeroaxis
set yzeroaxis 
set xtics rotate
set xlabel "Δt(s)"
set ylabel "θ(rad)"
set xrange[0:0.05]
set yrange[3.117:3.123]
p "Figura7.dat" u 1:2 w l t"θ(Δt)"
set ylabel "dθ(rad/s)"
set yrange[-0.066:-0.054]
p "Figura7.dat" u 1:3 w l t"dθ(Δt)"
set ylabel "E_{cin}(J)"
set yrange[0.00065:0.0013]
p "Figura7.dat" u 1:4 w l t"E_{cin}(Δt)"
set ylabel "E_{pot}(J)"
set yrange[3.9816:3.9822]
p "Figura7.dat" u 1:5 w l t"E_{pot}(Δt)"
unset multiplot
########################################################################