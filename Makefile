# If the compiler flags below don't work on your machine, then
# just don't use the makefile. No options are really needed.

zuo: zuo.c
	cc -O2 -Wall -g -o zuo zuo.c
