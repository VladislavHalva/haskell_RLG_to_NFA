# Makefile for FLP project 2019/2020
# Author: Vladislav Halva
# Login: xhalva04

all:
	ghc src/draft.hs -o plg-2-nka

clean: 
	rm plg-2-nka *.o *.hi