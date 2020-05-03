#FIGURA1################################################################
set title "Pèndul simple per petites oscil·lacions (N_{DAT}=1200)"
set term png
set output "P7-2016-fig1.png"
set xzeroaxis 
set yzeroaxis 
set xlabel "t(s)"
set ylabel "θ(rad)"
set xrange[0:18.25]
#set yrange[-0.3:0.4]
set key below

f(x)=0.1*cos(x*sqrt(3.711/1.25))
p "P7-2016-c2-res.dat" index 0 u 1:2 w l t"Euler Normal",\
  "" index 1 u 1:2 w l t"Euler Millorat",\
   f(x) w l t"θ(t)=0.1cos(ωt)"
   
#FIGURA2################################################################
set title "Pèndul simple per oscil·lacions grans(N_{DAT}=1500)"
set term png
set output "P7-2016-fig2.png"
set xzeroaxis
set yzeroaxis 
set xlabel "t(s)"
set ylabel "θ(rad)"
set xrange[0:18.25]
#set yrange[-25:10]
set key below
p "P7-2016-c2-res.dat" index 2 u 1:2 w l t"Euler Normal",\
  "" index 3 u 1:2 w l t"Euler Millorat"

#FIGURA3################################################################
set title "Grans oscil·lacions a l'espai de fase (N_{DAT}=1500)"
set term png
set output "P7-2016-fig3.png"
set xzeroaxis
set yzeroaxis 
set xlabel "θ(rad)"
set ylabel "dθ (rad/s)"
set xrange[-30:5]
#set yrange[-6:8]
set key below
p "P7-2016-c2-res.dat" index 2 u 2:3 w l t"Euler Normal",\
  "" index 3 u 2:3 w l t"Euler Millorat"
  
#FIGURA4################################################################
set term png
set output "P7-2016-fig4.png"
set title "Evolució E_{cin} i E_{T}"
set xzeroaxis
set yzeroaxis 
set xlabel "t(s)"
set ylabel "E(J)"
set xrange[0:18.25]
#set yrange[-3:3]
set key below
#set label "C.I.: θ(0)=π-0.015, dθ(0)=0.1"
p "P7-2016-c2-res.dat" index 4 u 1:4 w l t"E_{cin} E.N.",\
   "" index 4 u 1:6 w l t"E_{T} E.N.",\
   "" index 5 u 1:4 w l t"E_{cin} E.M.",\
   "" index 5 u 1:6 w l t"E_{T} E.M."

#FIGURA5################################################################
set term png
set output "P7-2016-fig5.png"
set multiplot layout 2, 1 title "Trajectòries a l'espai fàsic (N_{DAT}=5000)"
set xzeroaxis
set yzeroaxis 
set xlabel "θ(rad)"
set ylabel "dθ (rad/s)"
unset xrange
#set yrange[-6:8]
set key below
set title "C.I.: θ(0)=0, dθ(0)=2ω_{N}+0.1"
p "P7-2016-c2-res.dat" index 6 u 2:3 w p t""
  
set title "C.I.: θ(0)=0, dθ(0)=2ω_{N}-0.1"  
p "P7-2016-c2-res.dat" index 7 u 2:3 w p t""
  
unset multiplot 
   
  
#FIGURA6################################################################  
unset title
set term png
set output "P7-2016-fig6.png"
set multiplot layout 2, 2 title "E_{T} per diferents valors de N_{DAT}"
set xzeroaxis
set yzeroaxis 
set xlabel "t(s)"
set ylabel "E(J)"
set xrange[0:29.2]
set yrange[5.5:8.5]
p "P7-2016-c2-res.dat" index 8 u 1:6 w l t"N_{DAT}=300 pasos"
p "P7-2016-c2-res.dat" index 9 u 1:6 w l t"N_{DAT}=800 pasos"
p "P7-2016-c2-res.dat" index 10 u 1:6 w l t"N_{DAT}=3000 pasos"
p "P7-2016-c2-res.dat" index 11 u 1:6 w l t"N_{DAT}=45000 pasos"
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
unset xrange
unset yrange
#set yrange[3.0350115933563901:3.007459]
p "P7-2016-c2-resF.dat" u 1:2 w l t"θ(Δt)"
set ylabel "dθ(rad/s)"
#set yrange[0.0213:-0.154165]
set ytics 0.04
p "P7-2016-c2-resF.dat" u 1:3 w l t"dθ(Δt)"
set ytics auto
set ylabel "E_{cin}(J)"
#set yrange[0.000534:0.02786]
p "P7-2016-c2-resF.dat" u 1:4 w l t"E_{cin}(Δt)"
set ylabel "E_{pot}(J)"
#set yrange[6.9186419085918089:6.8956245173060573]
p "P7-2016-c2-resF.dat" u 1:5 w l t"E_{pot}(Δt)"
unset multiplot
########################################################################