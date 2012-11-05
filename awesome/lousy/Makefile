LDFLAGS += -lxcb -lxcb-screensaver
CFLAGS  += -fPIC -O2

all: lousy.so

lousy.so: lousy_wrap.o lousy.o
	gcc -shared -o $@ $^ $(LDFLAGS)

%.o: %.c
	gcc -c -o $@ $^ $(CFLAGS)

lousy_wrap.c: lousy.i
	swig -lua $^

.PHONY: clean
clean:
	rm -f *.o *.so lousy_wrap.c
