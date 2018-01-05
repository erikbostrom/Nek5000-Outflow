grep -i 'ErrXY' logfile > ErrXY.dat
gnuplot<<EOF
set terminal wxt size 500,500 persist
set yrange [10e-15:10e-1]
set logscale y; set size square 
set title 'CBC1, N=12'
set xlabel 'time t'
set ylabel 'L2 error'
plot "ErrXY.dat" u 2:3 w l notitle 
EOF
