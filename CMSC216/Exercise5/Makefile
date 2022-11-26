# provide a makefile with a target called shell_jr that 
# creates the executable shell_jr
CS = gcc
CSFLAGS = -ansi -Wall -g -O0 -Wwrite-strings -Wshadow -pedantic-errors -fstack-protector-all

shell_jr: shell_jr.o
	$(CS) $(CSFLAGS) shell_jr.c -o shell_jr
clean:
	rm -f *.o shell_jr
