CC=clang++
CFLAGS=-std=c++11 -D_GLIBCXX_DEBUG -g -fsanitize=address
GHC=ghc

all: J Jinter

J: J.cpp
	$(CC) $(CFLAGS) J.cpp

Jinter: J_interpolation.hs
	$(GHC) J_interpolation -o J_interpolation.out
