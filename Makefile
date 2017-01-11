all:
	gcc leastsq.c -lgsl -I/usr/include/gsl -lgslcblas -o leastsq
