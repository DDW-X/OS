[BITS 64]
section .text
global persist_firmware

persist_firmware:
    ; کد دستکاری NVRAM یا SPI
    ret