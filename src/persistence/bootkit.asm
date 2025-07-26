[BITS 64]
section .text
global persist_boot

persist_boot:
    ; کد جای‌گذاری در MBR/VBR
    ret