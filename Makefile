CC=gcc -O2 -pedantic -Wall #-Wno-unused-function
R=R

SOURCES = dframe.c


INCLUDE=-Ikx -I/usr/share/R/include
KX_OBJS=kx/l64/c.o

all: shared

shared: $(SOURCES)
	$(CC) -fPIC -shared -DKXVER=3 $(INCLUDE) -o test.so $^ $(KX_OBJS)

.c.o:
	$(CC) -c -O2 -Wall $(INCLUDE) $< -o $@
