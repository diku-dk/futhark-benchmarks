CC?=cc
CFLAGS?=-Wall -Wextra -pedantic -O3

all: pbbs2fut fut2pbbs

%: %.c
	$(CC) -o $@ $(CFLAGS) $^
