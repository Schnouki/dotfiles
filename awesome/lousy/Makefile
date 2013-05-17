LDFLAGS += -lxcb -lxcb-randr -lxcb-screensaver -lxcb-util
CFLAGS  += -fPIC -O2

all: lousy.so

lousy.so: lousy_wrap.o lousy.o backlight.o
	gcc -shared -o $@ $^ $(LDFLAGS)

%.o: %.c
	gcc -c -o $@ $^ $(CFLAGS)

lousy_wrap.c: lousy.i
	swig -lua $^

.PHONY: clean
clean:
	rm -f *.o *.so lousy_wrap.c
