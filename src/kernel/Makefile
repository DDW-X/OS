# Kernel module Makefile
obj-m := deepsick.o
deepsick-objs := deepsick_module.o encryption.o

KDIR ?= /lib/modules/$(shell uname -r)/build
PWD := $(shell pwd)

EXTRA_CFLAGS += -I$(PWD)/../../config -DDEBUG

all:
	$(MAKE) -C $(KDIR) M=$(PWD) modules

clean:
	$(MAKE) -C $(KDIR) M=$(PWD) clean
	rm -f *.o *.ko *.mod.c modules.order Module.symvers

install:
	sudo insmod deepsick.ko

uninstall:
	sudo rmmod deepsick
	