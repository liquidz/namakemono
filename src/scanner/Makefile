.PHONY: all clean

CC = gcc
LEX = flex

all: lex.yy.c nmk-scan

nmk-scan: lex.yy.c
	$(CC) $< -o $@ -lfl

lex.yy.c: namakemono.lex
	$(LEX) $<

clean:
	rm -f nmk-scan
