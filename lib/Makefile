# the compiler: gcc for C program
CC = gcc

# compiler flags:
# add -DLOGCCALLS if you want the verbose mode of C function calls (i.e. they print when they are called.)
CFLAGS=-Wall

# define any libraries to link:
LIBS =
# LFLAGS=-static

SRCS =mongolib.c

all: $(SRCS)
	$(CC) $(CFLAGS) -c $(SRCS) $(LFLAGS) $(LIBS)

clean:
	$(RM) *.o *~  a.out
