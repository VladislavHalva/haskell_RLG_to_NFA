# FIT VUT Brno, FLP 2019/2020 
# Assignment: PLG-2-NKA
# Author: Vladislav Halva
# Login: xhalva04

all:
	ghc -isrc -o plg-2-nka src/MainG2FA.hs 

clean: 
	rm plg-2-nka *.o *.hi
